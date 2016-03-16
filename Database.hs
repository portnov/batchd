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
import           Database.Persist.TH
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Types

getPool :: IO Sql.ConnectionPool
getPool = runStdoutLoggingT (Sqlite.createSqlitePool "test.db" 4)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
JobParam
  jobId JobId
  name String
  value String
  UniqParam jobId name

Job
  type String
  queueName String
  seq Int
  status JobStatus default=New
  UniqJobSeq queueName seq

Queue
  name String
  scheduleName String
  Primary name

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
    jiType :: String,
    jiSeq :: Int,
    jiStatus :: JobStatus,
    jiParams :: JobParamInfo
  }
  deriving (Generic)


instance ToJSON JobInfo where
  toJSON = genericToJSON (defaultOptions {fieldLabelModifier = camelCaseToUnderscore . stripPrefix "ji"})

instance FromJSON JobInfo where
  parseJSON (Object v) =
    JobInfo
      <$> v .: "type"
      <*> v .:? "seq" .!= 0
      <*> v .:? "status" .!= New
      <*> v .:? "params" .!= M.empty
  parseJSON invalid = typeMismatch "job" invalid

-- instance FromJSON JobParamInfo where
--   parseJSON (Object v) = do
--     let lst = H.toList v
--         lst' = [(T.unpack k, T.unpack v) | (k,v) <- lst]
--     return $ M.fromList lst'
--   parseJSON invalid = typeMismatch "job param" invalid

loadJob :: Key Job -> DB JobInfo
loadJob jid = do
  mbJob <- get jid
  case mbJob of
    Nothing -> throwR JobNotExists
    Just j -> do
      ps <- selectList [JobParamJobId ==. jid] []
      let params = M.fromList [(jobParamName p, jobParamValue p) | p <- map entityVal ps]
      return $ JobInfo (jobType j) (jobSeq j) (jobStatus j) params

getAllQueues :: DB [Entity Queue]
getAllQueues = selectList [] []

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
  jes <- getJobs qname mbStatus
  forM jes $ \je -> do
    loadJob (entityKey je)

equals = (E.==.)
infix 4 `equals`

getLastJobSeq :: String -> DB Int
getLastJobSeq qid = do
  lst <- E.select $
         E.from $ \job -> do
         E.where_ (job ^. JobQueueName `equals` E.val qid)
         return $ E.max_ (job ^. JobSeq)
  case map E.unValue lst of
    (Just r:_) -> return r
    _ -> return 0

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

addQueue :: String -> String -> DB (Key Queue)
addQueue name scheduleId = do
  r <- insertUnique $ Queue name scheduleId
  case r of
    Just qid -> return qid
    Nothing -> throwR QueueExists

getQueue :: String -> DB (Maybe Queue)
getQueue name = get (QueueKey name)

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

