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

module Common.Data where

import GHC.Generics

import Data.Time
import Data.Dates
import Database.Persist
import Data.Maybe
import Data.Aeson
import qualified Data.Text as T

import           Database.Persist.TH
import System.Exit

import Common.Types

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
  Primary jobId time

Queue
  name String
  title String default=''
  enabled Bool default=True
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
  Foreign User user userName
  Foreign Queue queue queueName
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
    uTitle    <- parseUpdate QueueTitle "title" o
    uEnable   <- parseUpdate QueueEnabled "enabled" o
    uHostName <- parseUpdate' QueueHostName "host_name" o
    return $ catMaybes [uEnable, uTitle, uSchedule, uHostName]

instance FromJSON [Update Job] where
  parseJSON o = do
    uQueue <- parseUpdate JobQueueName "queue_name" o
    uStatus <- parseUpdate JobStatus "status" o
    uHost   <- parseUpdate' JobHostName "host_name" o
    return $ catMaybes [uQueue, uStatus, uHost]

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

