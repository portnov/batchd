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
module Database where

import Control.Monad
import Data.Generics
--import Data.List
import Data.Maybe
import Data.Dates
import Data.Time

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobParam
  jobId JobId
  name String
  value String
  UniqParam jobId name

Job
  type String
  queueId QueueId
  seq Int
  UniqJobSeq queueId seq

Queue
  name String
  scheduleId ScheduleId
  UniqQueue name

Schedule
  name String

ScheduleTime
  scheduleId ScheduleId
  begin TimeOfDay
  end TimeOfDay

ScheduleWeekDay
  scheduleId ScheduleId
  weekDay WeekDay
|]

deriving instance Eq ScheduleTime
deriving instance Show ScheduleTime

data JobInfo = JobInfo {
    jiJob :: Job,
    jiParams :: [JobParam]
  }

loadJob :: Key Job -> DB JobInfo
loadJob jid = do
  mbJob <- get jid
  case mbJob of
    Nothing -> throwR JobNotExists
    Just j -> do
      ps <- selectList [JobParamJobId ==. jid] []
      let params = map entityVal ps
      return $ JobInfo j params

getAllQueues :: DB [Entity Queue]
getAllQueues = selectList [] []

getAllJobs :: Key Queue -> DB [Entity Job]
getAllJobs qid = selectList [JobQueueId ==. qid] [Asc JobSeq]

equals = (E.==.)
infix 4 `equals`

getLastJobSeq :: Key Queue -> DB Int
getLastJobSeq qid = do
  lst <- E.select $
         E.from $ \job -> do
         E.where_ (job ^. JobQueueId `equals` E.val qid)
         return $ E.max_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just r:_) -> return r
    _ -> return 0

deleteQueue :: String -> Bool -> DB ()
deleteQueue name forced = do
  mbQueue <- getBy (UniqQueue name)
  case mbQueue of
    Nothing -> throwR QueueNotExists
    Just qe -> do
      js <- selectFirst [JobQueueId ==. entityKey qe] []
      if isNothing js || forced
        then delete (entityKey qe)
        else throwR QueueNotEmpty

addQueue :: String -> Key Schedule -> DB (Key Queue)
addQueue name scheduleId = do
  r <- insertUnique $ Queue name scheduleId
  case r of
    Just qid -> return qid
    Nothing -> throwR QueueExists

getQueue :: String -> DB (Maybe (Entity Queue))
getQueue name = getBy (UniqQueue name)

enqueue :: String -> JobInfo -> DB (Key Job)
enqueue qname jinfo = do
  mbQueue <- getQueue qname
  case mbQueue of
    Nothing -> throwR QueueNotExists
    Just qe -> do
      seq <- getLastJobSeq (entityKey qe)
      let job = (jiJob jinfo) {jobQueueId = entityKey qe, jobSeq = seq+1}
      jid <- insert job
      forM_ (jiParams jinfo) $ \param -> do
        let param' = param {jobParamJobId = jid}
        insert_ param'
      return jid

