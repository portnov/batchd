{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, RecordWildCards, DeriveGeneric #-}

module Daemon.Schedule where

import Control.Monad.Reader
import Data.Maybe
import Database.Persist

import System.Batchd.Common.Types
import System.Batchd.Daemon.Types
import Common.Data
import Common.Schedule

loadSchedule :: Key Schedule -> DB ScheduleInfo
loadSchedule scheduleId@(ScheduleKey sname) = do
  mbSchedule <- get scheduleId
  case mbSchedule of
    Nothing -> throwR (ScheduleNotExists sname)
    Just s -> do
      let name = scheduleName s
      ts <- selectList [ScheduleTimeScheduleName ==. sname] []
      let times = map (toPeriod . entityVal) ts
      ws <- selectList [ScheduleWeekDayScheduleName ==. sname] []
      let weekdays = map (scheduleWeekDayWeekDay . entityVal) ws
      return $ ScheduleInfo {
                 sName = name,
                 sWeekdays = if null weekdays then Nothing else Just weekdays,
                 sTime = if null times then Nothing else Just times
               }
        
addSchedule :: ScheduleInfo -> DB String
addSchedule si = do
  ScheduleKey sid <- insert $ Schedule (sName si)
  case sWeekdays si of
    Nothing -> return ()
    Just weekdays ->
      forM_ weekdays $ \wd -> do
        insert_ $ ScheduleWeekDay sid wd
  case sTime si of
    Nothing -> return ()
    Just times -> 
      forM_ times $ \(Period {..}) -> do
        let time = ScheduleTime sid periodBegin periodEnd
        insert_ time
  return sid

loadAllSchedules :: DB [ScheduleInfo]
loadAllSchedules = do
  sids <- selectKeysList [] []
  forM sids $ \sid -> loadSchedule sid

removeSchedule :: String -> Bool -> DB ()
removeSchedule name forced = do
  qs <- selectFirst [QueueScheduleName ==. name] []
  let key = ScheduleKey name
  if isNothing qs || forced
    then do
      deleteWhere [ScheduleTimeScheduleName ==. name]
      deleteWhere [ScheduleWeekDayScheduleName ==. name]
      deleteCascade key
    else throwR ScheduleUsed

