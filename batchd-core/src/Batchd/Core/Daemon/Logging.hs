{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
-- | This module contains utilities for logging, used by batchd daemon.
-- This also reexports required modules from @heavy-logger@ package.
module Batchd.Core.Daemon.Logging
  (
    translateString,
    logIO, debugIO, infoIO, reportErrorIO,
    getLoggingSettings,
    -- * Reexports
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Level,
    module System.Log.Heavy.Util,
    module System.Log.Heavy.TH,
    module System.Log.Heavy.Backends
  ) where

import Control.Monad (when)
import qualified Control.Monad.Trans as Trans
import Control.Monad.Trans (MonadIO)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F
import Language.Haskell.TH hiding (reportError)
import Language.Haskell.TH.Lift
import System.Log.Heavy
import System.Log.Heavy.Types
import System.Log.Heavy.Level
import System.Log.Heavy.Util
import System.Log.Heavy.TH
import System.Log.Heavy.Backends
import Text.Localize (translate, Localized)

import Batchd.Core.Common.Types

deriveLift ''DaemonMode

deriveLift ''DbDriver

deriveLift ''AuthMode
deriveLift ''LogTarget
deriveLift ''F.FormatItem
deriveLift ''F.Format
deriveLift ''LogConfig
deriveLift ''GlobalConfig

-- | Variant of @translate@, which takes String argument.
translateString :: Localized m => String -> m TL.Text
translateString str = translate $ stringToBstr str

-- | Write a message to log within IO monad.
logIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> Level -> TL.Text -> vars -> m ()
logIO lts loc level msg vars = Trans.liftIO $
  do
    let src = splitDots (loc_module loc)
    let message = LogMessage level src loc msg vars []
    when (checkContextFilter (ltsContext lts) message) $ do
        ltsLogger lts message

-- | Write a debug message to log within IO monad.
debugIO :: (F.VarContainer vars, MonadIO m)
        => LoggingTState                     -- ^ Logging state
        -> Loc                               -- ^ Message location in Haskell source. Usually filled by @\$(here)@ TH macros.
        -> TL.Text                           -- ^ Message string, with placeholders if needed.
        -> vars                              -- ^ Message variables. Use @()@ if you do not have variables.
        -> m ()
debugIO lts loc = logIO lts loc debug_level

-- | Write an info message to log within IO monad.
infoIO :: (F.VarContainer vars, MonadIO m)
       => LoggingTState -- ^ Logging state
       -> Loc           -- ^ Message location in Haskell source. Usually filled by @\$(here)@ TH macros.
       -> TL.Text       -- ^ Message string, with placeholders if needed.
       -> vars          -- ^ Message variables. Use @()@ if you do not have variables.                   
       -> m ()
infoIO lts loc = logIO lts loc info_level

-- | Write an error message to log within IO monad.
reportErrorIO :: (F.VarContainer vars, MonadIO m)
              => LoggingTState -- ^ Logging state
              -> Loc           -- ^ Message location in Haskell source. Usually filled by @\$(here)@ TH macros.
              -> TL.Text       -- ^ Message string, with placeholders if needed.
              -> vars          -- ^ Message variables. Use @()@ if you do not have variables.                   
              -> m ()
reportErrorIO lts loc = logIO lts loc error_level

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

