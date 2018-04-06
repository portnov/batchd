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
-- | This module contains data type definitions for most entities stored in DB.
module Batchd.Common.Data where

import GHC.Generics

import Data.Time
import Data.Dates
import Database.Persist
import Data.Maybe
import Data.Int
import Data.Aeson as Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Data.Generics hiding (Generic)

import Database.Persist.TH
import System.Exit

import Batchd.Common.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade sqlSettings] [persistLowerCase|
JobParam
  jobId JobId
  name TL.Text
  value T.Text
  UniqParam jobId name

Job
  typeName String
  queueName String
  seq Int
  userName String default='root'
  createTime UTCTime default=CURRENT_TIMESTAMP
  startTime UTCTime Maybe
  status JobStatus default='New'
  tryCount Int default=0
  hostName String Maybe
  notes String Maybe
  UniqJobSeq queueName seq
  Foreign User user userName

JobResult
  jobId JobId
  time UTCTime default=CURRENT_TIMESTAMP
  exitCode ExitCode Maybe
  stdout T.Text sqltype=TEXT
  stderr T.Text sqltype=TEXT
  Primary jobId time

Queue
  name String
  title String default=''
  enabled Bool default=True
  scheduleName String
  hostName String Maybe
  autostartJobCount Int Maybe
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

MetricRecord
  name T.Text
  time UTCTime
  daemon DaemonMode
  kind MetricKind
  value Int64 Maybe
  text T.Text Maybe
  mean Double Maybe
  variance Double Maybe
  count Int64 Maybe
  sum Double Maybe
  min Double Maybe
  max Double Maybe
  Primary name time daemon

User
  name String
  pwdHash String
  salt String
  Primary name

UserPermission
  userName String
  permission Permission
  queueName String Maybe
  typeName String Maybe
  hostName String Maybe
  Foreign User user userName
  Foreign Queue queue queueName
|]

deriving instance Eq ScheduleTime
deriving instance Show ScheduleTime

deriving instance Generic Queue
deriving instance Generic MetricKind
deriving instance Generic MetricRecord

instance ToJSON Queue where
  toJSON = genericToJSON (jsonOptions "queue")

instance FromJSON Queue where
  parseJSON = genericParseJSON (jsonOptions "queue")

newtype UpdateList a = UpdateList [Update a]

instance FromJSON (UpdateList ScheduleTime) where
  parseJSON o = do
    uBegin <- parseUpdate ScheduleTimeBegin "begin" o
    uEnd   <- parseUpdate ScheduleTimeEnd   "end"   o
    return $ UpdateList $ catMaybes [uBegin, uEnd]

instance FromJSON (UpdateList Queue) where
  parseJSON o = do
    uSchedule <- parseUpdate QueueScheduleName "schedule_name" o
    uTitle    <- parseUpdate QueueTitle "title" o
    uEnable   <- parseUpdate QueueEnabled "enabled" o
    uHostName <- parseUpdateStar QueueHostName "host_name" o
    uAutostart <- parseUpdateMaybe QueueAutostartJobCount "autostart_job_count" o
    return $ UpdateList $ catMaybes [uEnable, uTitle, uSchedule, uHostName, uAutostart]

instance FromJSON (UpdateList Job) where
  parseJSON o = do
    uQueue <- parseUpdate JobQueueName "queue_name" o
    uStatus <- parseUpdate JobStatus "status" o
    uHost   <- parseUpdateStar JobHostName "host_name" o
    uNotes  <- parseUpdateMaybe JobNotes "notes" o
    uStart  <- parseUpdateMaybe JobStartTime "start_time" o
    return $ UpdateList $ catMaybes [uQueue, uStatus, uHost, uNotes, uStart]

deriving instance Generic JobResult
deriving instance Generic UserPermission

instance ToJSON JobResult where
  toJSON = genericToJSON (jsonOptions "jobResult")

instance FromJSON JobResult where
  parseJSON = genericParseJSON (jsonOptions "jobResult")

instance ToJSON UserPermission where
  toJSON = genericToJSON (jsonOptions "userPermission")

instance FromJSON UserPermission where
  parseJSON = genericParseJSON (jsonOptions "userPermission")

data MoveAction =
    First
  | More
  | Less
  | Last
  deriving (Eq, Show, Data, Typeable, Generic)

instance FromJSON MoveAction where
  parseJSON (Aeson.String "first") = return First
  parseJSON (Aeson.String "more") = return More
  parseJSON (Aeson.String "less") = return Less
  parseJSON (Aeson.String "last") = return Last
  parseJSON invalid = typeMismatch "job priority direction" invalid

instance ToJSON MoveAction where
  toJSON First = toJSON ("first" :: String)
  toJSON More = toJSON ("more" :: String)
  toJSON Less = toJSON ("less" :: String)
  toJSON Last = toJSON ("last" :: String)

data JobUpdate =
    Prioritize MoveAction
  | Move String
  | UpdateJob (UpdateList Job)

instance FromJSON JobUpdate where
  parseJSON o@(Object v) = do
    pr <- v .:? "priority"
    case pr of
      Just action -> return $ Prioritize action
      Nothing -> do
        mv <- v .:? "move"
        case mv of
          Just qname -> return $ Move qname
          Nothing -> do
            upd <- parseJSON o
            return $ UpdateJob upd
  parseJSON invalid = typeMismatch "job update query" invalid

instance ToJSON MetricKind where
  toJSON Counter = Aeson.String "c"
  toJSON Gauge = Aeson.String "g"
  toJSON Label = Aeson.String "l"
  toJSON Distribution = Aeson.String "d"

instance FromJSON MetricKind where
  parseJSON (Aeson.String "c") = pure Counter
  parseJSON (Aeson.String "g") = pure Gauge
  parseJSON (Aeson.String "l") = pure Label
  parseJSON (Aeson.String "d") = pure Distribution
  parseJSON v = typeMismatch "metric kind" v

instance ToJSON MetricRecord where
  toJSON = genericToJSON (jsonOptions "metricRecord")

instance FromJSON MetricRecord where
  parseJSON = genericParseJSON (jsonOptions "metricRecord")

instance VarContainer MetricRecord where
  lookupVar "value" r = Just $ Variable $ metricRecordValue r
  lookupVar "text" r = Just $ Variable $ metricRecordText r
  lookupVar "mean" r = Just $ Variable $ metricRecordMean r
  lookupVar "variance" r = Just $ Variable $ metricRecordVariance r
  lookupVar "count" r = Just $ Variable $ metricRecordCount r
  lookupVar "sum" r = Just $ Variable $ metricRecordSum r
  lookupVar "min" r = Just $ Variable $ metricRecordMin r
  lookupVar "max" r = Just $ Variable $ metricRecordMax r
  lookupVar _ _ = Nothing

instance ClosedVarContainer MetricRecord where
  allVarNames _ = ["value", "text", "mean", "variance", "count", "sum", "min", "max"]

-- instance ToJSON JobUpdate where
--   toJSON (Prioritize action) = object ["priority" .= action]
--   toJSON (Move qname) = object ["move" .= qname]
--   toJSON (UpdateJob lst) = toJSON lst

