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
import Control.Applicative
import Control.Monad
import Data.Generics hiding (Generic)
import Data.List (isPrefixOf)
import Data.Char
import Data.Maybe
import Data.Dates
import Data.Time
import Data.Int
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import           Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import           Database.Persist
import           Database.Persist.Sql as Sql
import           Database.Persist.Sqlite as Sqlite
import           Database.Persist.Postgresql as Postgres
import           Database.Persist.TH
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import System.Exit

import Types

getPool :: IO Sql.ConnectionPool
-- getPool = runStdoutLoggingT (Sqlite.createSqlitePool "test.db" 4)
getPool = runStdoutLoggingT (Postgres.createPostgresqlPool "host=localhost port=5432 user=batchd password=batchd" 4)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobParam
  jobId JobId
  name String
  value String
  UniqParam jobId name

Job
  typeName String
  queueName String
  seq Int
  status JobStatus default='New'
  UniqJobSeq queueName seq

JobResult
  jobId JobId
  exitCode ExitCode
  stdout T.Text sqltype=TEXT
  stderr T.Text sqltype=TEXT
  Primary jobId

Queue
  name String
  scheduleName String
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

type JobParamInfo = M.Map String String

data JobInfo = JobInfo {
    jiId :: Int64,
    jiQueue :: String,
    jiType :: String,
    jiSeq :: Int,
    jiStatus :: JobStatus,
    jiParams :: JobParamInfo
  }
  deriving (Generic, Show)

instance ToJSON JobInfo where
  toJSON = genericToJSON (jsonOptions "ji")

instance FromJSON JobInfo where
  parseJSON (Object v) =
    JobInfo
      <$> v .:? "id" .!= 0
      <*> v .:? "queue" .!= ""
      <*> v .: "type"
      <*> v .:? "seq" .!= 0
      <*> v .:? "status" .!= New
      <*> v .:? "params" .!= M.empty
  parseJSON invalid = typeMismatch "job" invalid

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
    return $ catMaybes [uSchedule]

deriving instance Generic ExitCode
instance ToJSON ExitCode

deriving instance Generic JobResult

instance ToJSON JobResult where
  toJSON = genericToJSON (jsonOptions "jobResult")

loadJob :: Key Job -> DB JobInfo
loadJob jkey@(JobKey (SqlBackendKey jid)) = do
  mbJob <- get jkey
  case mbJob of
    Nothing -> throwR JobNotExists
    Just j -> do
      ps <- selectList [JobParamJobId ==. jkey] []
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ JobInfo jid (jobQueueName j) (jobTypeName j) (jobSeq j) (jobStatus j) params

lockJob :: JobInfo -> DB ()
lockJob ji = do
  let qname = jiQueue ji
      seq = jiSeq ji
  E.select $
    E.from $ \job -> do
    E.where_ $ (job ^. JobQueueName `equals` E.val qname) `eand` (job ^. JobSeq `equals` E.val seq)
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
      return $ JobInfo jid (jobQueueName j) (jobTypeName j) (jobSeq j) (jobStatus j) params

setJobStatus :: JobInfo -> JobStatus -> DB ()
setJobStatus ji status = do
  updateWhere [JobQueueName ==. jiQueue ji, JobSeq ==. jiSeq ji] [JobStatus =. status]

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
  r <- insertUnique $ Queue name scheduleId
  case r of
    Just qid -> return qid
    Nothing -> throwR QueueExists

getQueue :: String -> DB (Maybe Queue)
getQueue name = get (QueueKey name)

updateQueue :: String -> [Update Queue] -> DB ()
updateQueue qname updates = do
  update (QueueKey qname) updates

enqueue :: String -> JobInfo -> DB (Key Job)
enqueue qname jinfo = do
  mbQueue <- getQueue qname
  case mbQueue of
    Nothing -> throwR QueueNotExists
    Just qe -> do
      seq <- getLastJobSeq qname
      let job = Job (jiType jinfo) qname (seq+1) (jiStatus jinfo)
      jid <- insert job
      forM_ (M.assocs $ jiParams jinfo) $ \(name,value) -> do
        let param = JobParam jid name value
        insert_ param
      return jid

removeJob :: String -> Int -> DB ()
removeJob qname jseq = do
    deleteBy $ UniqJobSeq qname jseq

