{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, RecordWildCards #-}

module Schedule where

import Control.Monad.Reader
import Data.Dates
import Data.Time
import Data.Generics
import Database.Persist
import Database.Persist.Sql

import Types
import Database

-- deriving instance Typeable Time
-- deriving instance Data Time

data ScheduleInfo =
  ScheduleInfo {
    sName :: String
  , sWeekdays :: Maybe [WeekDay]
  , sTime :: Maybe [ScheduleTime]
  }
  deriving (Eq, Show)

anytime :: ScheduleInfo
anytime = ScheduleInfo "anytime" Nothing Nothing

getTime :: DateTime -> TimeOfDay
getTime (DateTime {..}) = TimeOfDay hour minute (fromIntegral second)

allows :: ScheduleInfo -> DateTime -> Bool
allows (ScheduleInfo {..}) dt = weekdayOk && timeOk
  where
    weekdayOk = case sWeekdays of
                  Nothing -> True
                  Just weekdays -> dateWeekDay dt `elem` weekdays

    timeOk = case sTime of
               Nothing -> True
               Just lst -> any goodTime lst

    goodTime (ScheduleTime {..}) = 
                  getTime dt > scheduleTimeBegin && getTime dt <= scheduleTimeEnd

loadSchedule :: Key Schedule -> DB ScheduleInfo
loadSchedule scheduleId = do
  mbSchedule <- get scheduleId
  case mbSchedule of
    Nothing -> fail "No such schedule"
    Just s -> do
      let name = scheduleName s
      ts <- selectList [ScheduleTimeScheduleId ==. scheduleId] []
      let times = map entityVal ts
      ws <- selectList [ScheduleWeekDayScheduleId ==. scheduleId] []
      let weekdays = map (scheduleWeekDayWeekDay . entityVal) ws
      return $ ScheduleInfo {
                 sName = name,
                 sWeekdays = if null weekdays then Nothing else Just weekdays,
                 sTime = if null times then Nothing else Just times
               }
        
addSchedule :: ScheduleInfo -> DB (Key Schedule)
addSchedule si = do
  sid <- insert $ Schedule (sName si)
  case sWeekdays si of
    Nothing -> return ()
    Just weekdays ->
      forM_ weekdays $ \wd -> do
        insert_ $ ScheduleWeekDay sid wd
  case sTime si of
    Nothing -> return ()
    Just times -> 
      forM_ times $ \t ->
        insert_ $ t {scheduleTimeScheduleId = sid}
  return sid

