{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, RecordWildCards, DeriveGeneric #-}

module Schedule where

import GHC.Generics
import Control.Monad.Reader
import Data.Maybe
import Data.Dates
import Data.Dates.Formats
import Data.Time
import Database.Persist
import Data.Aeson
import Text.Parsec
import Text.Parsec.String

import CommonTypes
import Types
import Database

-- deriving instance Typeable Time
-- deriving instance Data Time

data Period =
  Period {
    periodBegin :: TimeOfDay,
    periodEnd :: TimeOfDay
  }
  deriving (Eq, Generic)

instance Show Period where
  show (Period begin end) = show begin ++ " -- " ++ show end

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

parsePeriod :: String -> Either ParseError Period
parsePeriod str = parse parser "(period description)" str 
  where
    format :: Format
    format = [HOUR True 2,Fixed True ":",MINUTE True 2,Fixed True ":",SECOND True 2]

    parser :: Parser Period
    parser = do
      begin <- formatParser format
      spaces
      end <- formatParser format
      return $ Period (getTime begin) (getTime end)

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
loadSchedule scheduleId@(ScheduleKey sname) = do
  mbSchedule <- get scheduleId
  case mbSchedule of
    Nothing -> fail "No such schedule"
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

