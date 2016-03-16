{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, RecordWildCards, DeriveGeneric #-}

module Schedule where

import GHC.Generics
import Control.Monad.Reader
import Data.Dates
import Data.Time
import Data.Generics hiding (Generic)
import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import Data.Aeson.Types

import Types
import Database

-- deriving instance Typeable Time
-- deriving instance Data Time

data Period =
  Period {
    periodBegin :: TimeOfDay,
    periodEnd :: TimeOfDay
  }
  deriving (Eq, Show, Generic)

toPeriod :: ScheduleTime -> Period
toPeriod (ScheduleTime {..}) = Period scheduleTimeBegin scheduleTimeEnd

instance ToJSON Period where
  toJSON = genericToJSON (jsonOptions "period")

instance FromJSON Period where
  parseJSON = genericParseJSON (jsonOptions "period")

data ScheduleInfo =
  ScheduleInfo {
    sName :: String
  , sWeekdays :: Maybe [WeekDay]
  , sTime :: Maybe [Period]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ScheduleInfo where
  toJSON = genericToJSON (jsonOptions "s")

instance FromJSON ScheduleInfo where
  parseJSON = genericParseJSON (jsonOptions "s")

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

    goodTime (Period {..}) = 
                  getTime dt > periodBegin && getTime dt <= periodEnd

loadSchedule :: Key Schedule -> DB ScheduleInfo
loadSchedule scheduleId = do
  mbSchedule <- get scheduleId
  case mbSchedule of
    Nothing -> fail "No such schedule"
    Just s -> do
      let name = scheduleName s
      ts <- selectList [ScheduleTimeScheduleId ==. scheduleId] []
      let times = map (toPeriod . entityVal) ts
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
      forM_ times $ \(Period {..}) -> do
        let time = ScheduleTime sid periodBegin periodEnd
        insert_ time
  return sid

loadAllSchedules :: DB [ScheduleInfo]
loadAllSchedules = do
  sids <- selectKeysList [] []
  forM sids $ \sid -> loadSchedule sid

