{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}

module Daemon.Logging
  (
    translateString,
    logIO, debugIO, infoIO, reportErrorIO,
    logDB, debugDB, infoDB, reportErrorDB,
    getLoggingSettings,
    module System.Log.Heavy.Types,
    module System.Log.Heavy.Level,
    module System.Log.Heavy.TH,
    module System.Log.Heavy.Backends
  ) where

import qualified Control.Monad.Trans as Trans
import Control.Monad.Reader hiding (lift)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F
import Language.Haskell.TH hiding (reportError)
import Language.Haskell.TH.Lift
import Control.Monad.Logger (LogLevel (..), liftLoc)
import System.Log.Heavy
import System.Log.Heavy.Types
import System.Log.Heavy.Level
import System.Log.Heavy.TH
import System.Log.Heavy.Backends
import qualified System.Posix.Syslog as Syslog
import Text.Localize (translate, Localized)
import Instances.TH.Lift

import Common.Types
import Daemon.Types

deriveLift ''DaemonMode

deriveLift ''DbDriver

deriveLift ''AuthMode
deriveLift ''LogTarget
deriveLift ''F.FormatItem
deriveLift ''F.Format
deriveLift ''LogConfig
deriveLift ''GlobalConfig

translateString :: Localized m => String -> m TL.Text
translateString str = translate $ stringToBstr str

logIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> Level -> TL.Text -> vars -> m ()
logIO lts loc level msg vars = Trans.liftIO $
  do
    let src = splitDots (loc_module loc)
    let message = LogMessage level src loc msg vars []
    when (checkContextFilter (ltsContext lts) message) $ do
        ltsLogger lts message

logDB :: Level -> Q Exp
logDB = putMessage

infoDB :: Q Exp
infoDB = info

debugDB :: Q Exp
debugDB = debug

reportErrorDB :: Q Exp
reportErrorDB = reportError

debugIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> TL.Text -> vars -> m ()
debugIO lts loc = logIO lts loc debug_level

infoIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> TL.Text -> vars -> m ()
infoIO lts loc = logIO lts loc info_level

reportErrorIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> TL.Text -> vars -> m ()
reportErrorIO lts loc = logIO lts loc error_level

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

