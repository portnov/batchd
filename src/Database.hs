{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database where

import GHC.Generics
import Control.Monad.Reader

import Data.Time
import Data.Dates
import Database.Persist
import Data.Maybe
import Data.Int
import Data.Time
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import           Database.Persist.Sql as Sql
import           Database.Persist.Sqlite as Sqlite
import           Database.Persist.Postgresql as Postgres
import           Database.Persist.TH
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import System.Exit

import CommonTypes
import Types

getPool :: GlobalConfig -> IO Sql.ConnectionPool
getPool cfg =
  case dbcDriver cfg of
    Sqlite -> (enableLogging cfg) (Sqlite.createSqlitePool (dbcConnectionString cfg) 4)
    PostgreSql -> do
        let str = TE.encodeUtf8 (dbcConnectionString cfg)
        (enableLogging cfg) (Postgres.createPostgresqlPool str 4)

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade sqlSettings] [persistLowerCase|
JobParam
  jobId JobId
  name String
  value String
  UniqParam jobId name

Job
  typeName String
  queueName String
  seq Int
  createTime UTCTime default=CURRENT_TIMESTAMP
  status JobStatus default='New'
  tryCount Int default=0
  hostName String Maybe
  UniqJobSeq queueName seq

JobResult
  jobId JobId
  time UTCTime default=CURRENT_TIMESTAMP
  exitCode ExitCode
  stdout T.Text sqltype=TEXT
  stderr T.Text sqltype=TEXT
  Primary jobId

Queue
  name String
  scheduleName String
  hostName String Maybe
  Primary name
  Foreign Schedule schedule scheduleName

Schedule
  name String
  Primary name

ScheduleTime
  scheduleName String
  begin TimeOfDay
  end TimeOfDay
  Foreign Schedule schedule scheduleName

ScheduleWeekDay
  scheduleName String
  weekDay WeekDay
  Foreign Schedule schedule scheduleName
|]

deriving instance Eq ScheduleTime
deriving instance Show ScheduleTime

deriving instance Generic Queue

instance ToJSON Queue where
  toJSON = genericToJSON (jsonOptions "queue")

instance FromJSON Queue where
  parseJSON = genericParseJSON (jsonOptions "queue")

instance FromJSON [Update ScheduleTime] where
  parseJSON o = do
    uBegin <- parseUpdate ScheduleTimeBegin "begin" o
    uEnd   <- parseUpdate ScheduleTimeEnd   "end"   o
    return $ catMaybes [uBegin, uEnd]

instance FromJSON [Update Queue] where
  parseJSON o = do
    uSchedule <- parseUpdate QueueScheduleName "schedule_name" o
    uHostName <- parseUpdate' QueueHostName "host_name" o
    return $ catMaybes [uSchedule, uHostName]

deriving instance Generic ExitCode
instance ToJSON ExitCode

deriving instance Generic JobResult

instance ToJSON JobResult where
  toJSON = genericToJSON (jsonOptions "jobResult")

buildJobInfo :: Int64 -> Job -> JobParamInfo -> JobInfo
buildJobInfo jid j params =
   JobInfo {
      jiId = jid,
      jiQueue = jobQueueName j,
      jiType = jobTypeName j,
      jiTime = jobCreateTime j,
      jiSeq = jobSeq j,
      jiStatus = jobStatus j,
      jiTryCount = jobTryCount j,
      jiHostName = jobHostName j,
      jiParams = params
     }

loadJob :: Key Job -> DB JobInfo
loadJob jkey@(JobKey (SqlBackendKey jid)) = do
  mbJob <- get jkey
  case mbJob of
    Nothing -> throwR JobNotExists
    Just j -> do
      ps <- selectList [JobParamJobId ==. jkey] []
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ buildJobInfo jid j params

lockJob :: JobInfo -> DB ()
lockJob ji = do
  let qname = jiQueue ji
      seq = jiSeq ji
  E.select $
    E.from $ \job -> do
    E.where_ $ (job ^. JobQueueName `equals` E.val qname) `eand` (job ^. JobSeq `equals` E.val seq)
    E.locking E.ForUpdate
  return ()

lockQueue :: String -> DB ()
lockQueue qname = do
  E.select $
    E.from $ \queue -> do
    E.where_ (queue ^. QueueName `equals` E.val qname)
    E.locking E.ForUpdate
  return ()

loadJobSeq :: String -> Int -> DB JobInfo
loadJobSeq qname seq = do
  mbJe <- getBy (UniqJobSeq qname seq)
  case mbJe of
    Nothing -> throwR JobNotExists
    Just je -> do
      let JobKey (SqlBackendKey jid) = entityKey je
      ps <- selectList [JobParamJobId ==. entityKey je] []
      let j = entityVal je
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ buildJobInfo jid j params

setJobStatus :: JobInfo -> JobStatus -> DB ()
setJobStatus ji status = do
  updateWhere [JobQueueName ==. jiQueue ji, JobSeq ==. jiSeq ji] [JobStatus =. status]

increaseTryCount :: JobInfo -> DB Int
increaseTryCount ji = do
  let qname = jiQueue ji
      seq = jiSeq ji
  mbJe <- getBy (UniqJobSeq qname seq)
  case mbJe of
    Nothing -> throwR JobNotExists
    Just je -> do
      let count = jobTryCount (entityVal je)
          count' = count + 1
      updateWhere [JobQueueName ==. qname, JobSeq ==. seq] [JobTryCount =. count']
      return count'

moveToEnd :: JobInfo -> DB ()
moveToEnd ji = do
  let qname = jiQueue ji
      seq = jiSeq ji
  lockQueue qname
  mbJe <- getBy (UniqJobSeq qname seq)
  case mbJe of
    Nothing -> throwR JobNotExists
    Just je -> do
      lastSeq <- getLastJobSeq qname
      let seq' = lastSeq + 1
      updateWhere [JobQueueName ==. qname, JobSeq ==. seq] [JobSeq =. seq', JobStatus =. New]

getAllQueues :: DB [Entity Queue]
getAllQueues = selectList [] []

getAllQueues' :: DB [Queue]
getAllQueues' = map entityVal `fmap` selectList [] []

getAllJobs :: String -> DB [Entity Job]
getAllJobs qid = selectList [JobQueueName ==. qid] [Asc JobSeq]

getJobs :: String -> Maybe JobStatus -> DB [Entity Job]
getJobs qname mbStatus = do
  let filt = case mbStatus of
               Nothing -> []
               Just status -> [JobStatus ==. status]
  selectList ([JobQueueName ==. qname] ++ filt) [Asc JobSeq]

loadJobs :: String -> Maybe JobStatus -> DB [JobInfo]
loadJobs qname mbStatus = do
  mbQ <- getQueue qname
  case mbQ of
    Nothing -> throwR QueueNotExists
    Just _ -> do
        jes <- getJobs qname mbStatus
        forM jes $ \je -> do
          loadJob (entityKey je)

loadJobsByStatus :: Maybe JobStatus -> DB [JobInfo]
loadJobsByStatus mbStatus = do
  let filt = case mbStatus of
               Nothing -> []
               Just status -> [JobStatus ==. status]
  jes <- selectList filt [Asc JobId]
  forM jes $ \je -> do
      loadJob (entityKey je)

getJobResult :: Int64 -> DB JobResult
getJobResult i = do
  r <- get (JobResultKey (JobKey (SqlBackendKey i)))
  case r of
    Nothing -> throwR JobNotExists
    Just res -> return res

equals = (E.==.)
infix 4 `equals`

eand = (E.&&.)
infixr 3 `eand`

getLastJobSeq :: String -> DB Int
getLastJobSeq qid = do
  lst <- E.select $
         E.from $ \job -> do
         E.where_ (job ^. JobQueueName `equals` E.val qid)
         return $ E.max_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just r:_) -> return r
    _ -> return 0

getNextJob :: Key Queue -> DB (Maybe JobInfo)
getNextJob (QueueKey qname) = do
  lst <- E.select $
         E.from $ \job -> do
         E.where_ $ (job ^. JobQueueName `equals` E.val qname) `eand` (job ^. JobStatus `equals` E.val New)
         return $ E.min_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just seq:_) -> do
        jinfo <- loadJobSeq qname seq
        return $ Just jinfo
    _ -> return Nothing

deleteQueue :: String -> Bool -> DB ()
deleteQueue name forced = do
  mbQueue <- get (QueueKey name)
  case mbQueue of
    Nothing -> throwR QueueNotExists
    Just qe -> do
      js <- selectFirst [JobQueueName ==. name] []
      if isNothing js || forced
        then delete (QueueKey name)
        else throwR QueueNotEmpty

addQueue :: Queue -> DB (Key Queue)
addQueue q = insert q

addQueue' :: String -> String -> DB (Key Queue)
addQueue' name scheduleId = do
  r <- insertUnique $ Queue name scheduleId Nothing
  case r of
    Just qid -> return qid
    Nothing -> throwR QueueExists

getQueue :: String -> DB (Maybe Queue)
getQueue name = get (QueueKey name)

updateQueue :: String -> [Update Queue] -> DB ()
updateQueue qname updates = do
  update (QueueKey qname) updates

getQueueStats :: String -> DB (M.Map JobStatus Int)
getQueueStats qname = do
  let sql = "select status, count(1) from job where queue_name = ? group by status"
  pvs <- Sql.rawSql sql [toPersistValue qname]
  return $ M.fromList [(unSingle st, unSingle cnt) | (st,cnt) <- pvs]

getStats :: DB (M.Map String (M.Map JobStatus Int))
getStats = do
  queues <- getAllQueues'
  rs <- forM queues $ \q -> do
            st <- getQueueStats (queueName q)
            return (queueName q, st)
  return $ M.fromList rs

enqueue :: String -> JobInfo -> DB (Key Job)
enqueue qname jinfo = do
  mbQueue <- getQueue qname
  lockQueue qname
  case mbQueue of
    Nothing -> throwR QueueNotExists
    Just qe -> do
      seq <- getLastJobSeq qname
      now <- liftIO getCurrentTime
      let job = Job (jiType jinfo) qname (seq+1) now (jiStatus jinfo) (jiTryCount jinfo) (jiHostName jinfo)
      jid <- insert job
      forM_ (M.assocs $ jiParams jinfo) $ \(name,value) -> do
        let param = JobParam jid name value
        insert_ param
      return jid

removeJob :: String -> Int -> DB ()
removeJob qname jseq = do
    deleteCascadeWhere [JobQueueName ==. qname, JobSeq ==. jseq]

removeJobById :: Int64 -> DB ()
removeJobById jid = do
    let key = JobKey (SqlBackendKey jid)
    deleteCascade key

removeJobs :: String -> JobStatus -> DB ()
removeJobs qname status = do
    deleteCascadeWhere [JobQueueName ==. qname, JobStatus ==. status]

cleanupJobResults :: Int -> DB ()
cleanupJobResults days = do
  now <- liftIO $ getCurrentTime
  let delta = fromIntegral $ days * 24 * 3600
  let edge = addUTCTime (negate delta) now
  -- Delete jobs with obsolete results
  let deleteJobs = "delete from job where job_id in (select job_id from job_result where time < ?)"
  Sql.rawExecute deleteJobs [toPersistValue edge]
  -- Delete obsolete job results
  deleteCascadeWhere [JobResultTime <. edge]

-- cleanupJobs :: Int -> DB ()
-- cleanupJobs days = do
--   now <- liftIO $ getCurrentTime
--   let delta = fromIntegral $ days * 3600
--   let edge = addUTCTime (negate delta) now
  -- let deleteResults = "delete from job_result where job_id in (select job_id from job_result where time < ?)"
  -- Sql.rawExecute deleteResults [toPersistValue edge]
  -- let deleteJobs = "delete from job where time 

