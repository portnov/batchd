{-# LANGUAGE TemplateHaskell, TypeFamilies #-}

module Daemon.Logging where

import qualified Control.Monad.Trans as Trans
import Control.Monad.Reader hiding (lift)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Format.Heavy as F
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import Control.Monad.Logger (LogLevel (..), liftLoc)
import System.Log.Heavy
import System.Log.Heavy.Types
import System.Log.Heavy.Backends
-- import qualified System.Log.FastLogger as FL

import Common.Types
import Daemon.Types

deriveLift ''DaemonMode

deriveLift ''DbDriver

deriveLift ''AuthMode
deriveLift ''LogTarget
deriveLift ''LogConfig
deriveLift ''GlobalConfig

logConnectionM :: LogLevel -> Q Exp
logConnectionM level = [| \msg vars ->
  do
    let loc = $(qLocation >>= liftLoc)
    let src = splitDots (loc_module loc)
    let message = LogMessage $(lift level) src loc (TL.pack msg) vars
    Daemon $ logMessage message |]

here :: Q Exp
here = qLocation >>= liftLoc

logIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> LogLevel -> String -> vars -> m ()
logIO lts loc level msg vars = Trans.liftIO $
  do
    let src = splitDots (loc_module loc)
    let message = LogMessage level src loc (TL.pack msg) vars
    when (checkContextFilter (ltsContext lts) message) $ do
        ltsLogger lts message

debug :: Q Exp
debug = logConnectionM LevelDebug

info :: Q Exp
info = logConnectionM LevelInfo

reportError :: Q Exp
reportError = logConnectionM LevelError

logDB :: LogLevel -> Q Exp
logDB level = [| \msg vars ->
  do
    let loc = $(qLocation >>= liftLoc)
    let src = splitDots (loc_module loc)
    let message = LogMessage $(lift level) src loc (TL.pack msg) vars
    Trans.lift $ logMessage message |]

infoDB :: Q Exp
infoDB = logDB LevelInfo

debugDB :: Q Exp
debugDB = logDB LevelDebug

reportErrorDB :: Q Exp
reportErrorDB = logDB LevelError

debugIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> String -> vars -> m ()
debugIO lts loc = logIO lts loc LevelDebug

infoIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> String -> vars -> m ()
infoIO lts loc = logIO lts loc LevelInfo

reportErrorIO :: (F.VarContainer vars, MonadIO m) => LoggingTState -> Loc -> String -> vars -> m ()
reportErrorIO lts loc = logIO lts loc LevelError

getLoggingSettings :: GlobalConfig -> LoggingSettings
getLoggingSettings cfg =
    case lcTarget $ dbcLogging cfg of
      LogSyslog -> LoggingSettings $ Filtering fltr $ defaultSyslogSettings {ssIdent = "batchd"}
      LogStdout -> LoggingSettings $ Filtering fltr $ defStdoutSettings 
      LogStderr -> LoggingSettings $ Filtering fltr $ defStderrSettings 
      LogFile path -> LoggingSettings $ Filtering fltr $ defFileSettings path
  where
    fltr :: LogFilter
    fltr = map toFilter (lcFilter $ dbcLogging cfg) ++ [([], lcLevel $ dbcLogging cfg)]

    toFilter :: (String, LogLevel) -> (LogSource, LogLevel)
    toFilter (src, level) = (splitDots src, level)

