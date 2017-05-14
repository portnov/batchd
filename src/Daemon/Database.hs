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
module Daemon.Database where

import Control.Monad.Reader

import Data.Time
import Database.Persist
import Data.Maybe
import Data.Int
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

import           Database.Persist.Sql as Sql
import           Database.Persist.Sqlite as Sqlite
import           Database.Persist.Postgresql as Postgres
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Common.Types
import Daemon.Types
import Common.Data

getPool :: GlobalConfig -> IO Sql.ConnectionPool
getPool cfg =
  case dbcDriver cfg of
    Sqlite -> (enableLogging cfg) (Sqlite.createSqlitePool (dbcConnectionString cfg) 1)
    PostgreSql -> do
        let str = TE.encodeUtf8 (dbcConnectionString cfg)
        (enableLogging cfg) (Postgres.createPostgresqlPool str 10)

buildJobInfo :: Int64 -> Job -> Maybe JobResult -> JobParamInfo -> JobInfo
buildJobInfo jid j mbr params =
   JobInfo {
      jiId = jid,
      jiQueue = jobQueueName j,
      jiType = jobTypeName j,
      jiCreateTime = jobCreateTime j,
      jiSeq = jobSeq j,
      jiStatus = jobStatus j,
      jiTryCount = jobTryCount j,
      jiHostName = jobHostName j,
      jiResultTime = fmap jobResultTime mbr,
      jiExitCode = fmap jobResultExitCode mbr,
      jiStdout = fmap jobResultStdout mbr,
      jiStderr = fmap jobResultStderr mbr,
      jiParams = params
     }

loadJob :: Key Job -> DB JobInfo
loadJob jkey@(JobKey (SqlBackendKey jid)) = do
  mbJob <- get jkey
  case mbJob of
    Nothing -> throwR JobNotExists
    Just j -> do
      mbr <- selectFirst [JobResultJobId ==. jkey] [Desc JobResultTime]
      ps <- selectList [JobParamJobId ==. jkey] []
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ buildJobInfo jid j (fmap entityVal mbr) params

loadJob' :: Int64 -> DB JobInfo
loadJob' jid = loadJob (JobKey (SqlBackendKey jid))

canLock :: DB Bool
canLock = do
  -- Dirty hack: we can't do "for update" in sqlite
  backend <- ask
  return $ connRDBMS backend /= "sqlite"

lockJob :: JobInfo -> DB ()
lockJob ji = do
  ok <- canLock
  when ok $ do
    let qname = jiQueue ji
        seq = jiSeq ji
    E.select $
      E.from $ \job -> do
      E.where_ $ (job ^. JobQueueName `equals` E.val qname) `eand` (job ^. JobSeq `equals` E.val seq)
      E.locking E.ForUpdate
    return ()

lockQueue :: String -> DB ()
lockQueue qname = do
  ok <- canLock
  when ok $ do
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
      let jkey = entityKey je
      let JobKey (SqlBackendKey jid) = jkey
      mbr <- selectFirst [JobResultJobId ==. jkey] [Desc JobResultTime]
      ps <- selectList [JobParamJobId ==. jkey] []
      let j = entityVal je
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ buildJobInfo jid j (fmap entityVal mbr) params

updateJob :: Int64 -> [Update Job] -> DB ()
updateJob jid updates = do
  let jkey = JobKey (SqlBackendKey jid)
  update jkey updates

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

getEnabledQueues :: DB [Entity Queue]
getEnabledQueues = selectList [QueueEnabled ==. True] []

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
    Nothing -> throwR (QueueNotExists qname)
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
getJobResult jid = do
  let jkey = JobKey (SqlBackendKey jid)
  r <- selectFirst [JobResultJobId ==. jkey] [Desc JobResultTime]
  case r of
    Nothing -> throwR JobNotExists
    Just res -> return $ entityVal res

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
    Nothing -> throwR (QueueNotExists name)
    Just qe -> do
      js <- selectFirst [JobQueueName ==. name] []
      if isNothing js || forced
        then delete (QueueKey name)
        else throwR QueueNotEmpty

addQueue :: Queue -> DB (Key Queue)
addQueue q = do
  r <- getQueue (queueName q)
  case r of
    Nothing -> do
        mbSchedule <- get (queueSchedule q)
        case mbSchedule of
          Nothing -> throwR (ScheduleNotExists (queueScheduleName q))
          otherwise -> insert q
    Just _ -> throwR (QueueExists (queueName q))

addQueue' :: String -> String -> DB (Key Queue)
addQueue' name scheduleId = do
  r <- insertUnique $ Queue name name True scheduleId Nothing
  case r of
    Just qid -> return qid
    Nothing -> throwR $ QueueExists name

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
    Nothing -> throwR (QueueNotExists qname)
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
  -- let deleteParams = "delete from job_param where job_id in (select job_id from job_result where time < ?) and (select status from job where id = job_param.job_id) in ('Done', 'Failed')"
  -- Sql.rawExecute deleteParams [toPersistValue edge]
  -- liftIO $ putStrLn deleteParams
  -- let deleteJobs = "delete from job where id in (select job_id from job_result where time < ?) and status in ('Done', 'Failed')"
  -- liftIO $ putStrLn deleteJobs
  -- Sql.rawExecute deleteJobs [toPersistValue edge]
  -- Delete obsolete job results
  deleteCascadeWhere [JobResultTime <. edge]
  deleteCascadeWhere [JobCreateTime <. edge, JobStatus <-. [Done, Failed]]

-- cleanupJobs :: Int -> DB ()
-- cleanupJobs days = do
--   now <- liftIO $ getCurrentTime
--   let delta = fromIntegral $ days * 3600
--   let edge = addUTCTime (negate delta) now
  -- let deleteResults = "delete from job_result where job_id in (select job_id from job_result where time < ?)"
  -- Sql.rawExecute deleteResults [toPersistValue edge]
  -- let deleteJobs = "delete from job where time 

