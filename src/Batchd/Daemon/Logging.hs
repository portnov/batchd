{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
-- | This module contains utilities for logging, used by batchd daemon.
module Batchd.Daemon.Logging
  (
    getLoggingSettings
  ) where

import qualified Data.Text.Format.Heavy as F
import Language.Haskell.TH.Lift
import System.Log.Heavy
import System.Log.Heavy.TH () -- import instances only

import Batchd.Common.Types

deriveLift ''DaemonMode

deriveLift ''DbDriver

deriveLift ''AuthMode
deriveLift ''LogTarget
deriveLift ''F.FormatItem
deriveLift ''F.Format
deriveLift ''LogConfig
deriveLift ''MetricsConfig
deriveLift ''WebClientConfig
deriveLift ''ManagerConfig
deriveLift ''DispatcherConfig
deriveLift ''StorageConfig
deriveLift ''GlobalConfig

-- | Get logging settings from global config.
getLoggingSettings :: GlobalConfig -> LoggingSettings
getLoggingSettings cfg =
    case lcTarget $ dbcLogging cfg of
      LogSyslog -> LoggingSettings $ filtering fltr $ defaultSyslogSettings {ssIdent = "batchd", ssFormat = logFormat}
      LogStdout -> LoggingSettings $ filtering fltr $ defStdoutSettings {lsFormat = logFormat}
      LogStderr -> LoggingSettings $ filtering fltr $ defStderrSettings {lsFormat = logFormat}
      LogFile path -> LoggingSettings $ filtering fltr $ (defFileSettings path) {lsFormat = logFormat}
  where
    fltr :: LogFilter
    fltr = map toFilter (lcFilter $ dbcLogging cfg) ++ [([], lcLevel $ dbcLogging cfg)]

    logFormat = lcFormat (dbcLogging cfg)

    toFilter :: (String, Level) -> (LogSource, Level)
    toFilter (src, level) = (splitDots src, level)

