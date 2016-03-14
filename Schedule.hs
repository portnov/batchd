{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, RecordWildCards #-}

module Schedule where

import Data.Dates
import Data.Time
import Data.Generics

-- deriving instance Typeable Time
-- deriving instance Data Time

data TimePeriod =
  TimePeriod {
    tpFrom :: Time
  , tpTo :: Time
  }
  deriving (Eq, Show, Data, Typeable)

data Schedule =
  Schedule {
    sWeekdays :: Maybe [WeekDay]
  , sTime :: Maybe [TimePeriod]
  }
  deriving (Eq, Show, Data, Typeable)

anytime :: Schedule
anytime = Schedule Nothing Nothing

getTime :: DateTime -> Time
getTime (DateTime {..}) = Time hour minute second

allows :: Schedule -> DateTime -> Bool
allows (Schedule {..}) dt = weekdayOk && timeOk
  where
    weekdayOk = case sWeekdays of
                  Nothing -> True
                  Just weekdays -> dateWeekDay dt `elem` weekdays

    timeOk = case sTime of
               Nothing -> True
               Just lst -> any goodTime lst

    goodTime (TimePeriod {..}) =
                  getTime dt > tpFrom && getTime dt <= tpTo
