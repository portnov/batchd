{-# LANGUAGE TemplateHaskell #-}

module Daemon.Logging where

import qualified Control.Monad.Trans as Trans
import Control.Monad.Reader hiding (lift)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift
import Control.Monad.Logger (LogLevel (..), liftLoc)
import System.Log.Heavy
import qualified System.Log.FastLogger as F

import Common.Types
import Daemon.Types

deriveLift ''DaemonMode

deriveLift ''DbDriver

deriveLift ''AuthMode
deriveLift ''LogTarget
deriveLift ''LogConfig
deriveLift ''GlobalConfig

logConnectionM :: LogLevel -> Q Exp
logConnectionM level = [| \msg ->
  do
    let loc = $(qLocation >>= liftLoc)
    let src = splitDots (loc_module loc)
    let message = LogMessage $(lift level) src loc $ F.toLogStr msg
    Daemon $ logMessage message |]

here :: Q Exp
here = qLocation >>= liftLoc

logIO :: MonadIO m => Logger -> Loc -> LogLevel -> String -> m ()
logIO logger loc level msg = Trans.liftIO $
  do
    let src = splitDots (loc_module loc)
    let message = LogMessage level src loc $ F.toLogStr msg
    logger message

debug :: Q Exp
debug = logConnectionM LevelDebug

info :: Q Exp
info = logConnectionM LevelInfo

reportError :: Q Exp
reportError = logConnectionM LevelError

logDB :: LogLevel -> Q Exp
logDB level = [| \msg ->
  do
    let loc = $(qLocation >>= liftLoc)
    let src = splitDots (loc_module loc)
    let message = LogMessage $(lift level) src loc $ F.toLogStr msg
    Trans.lift $ logMessage message |]

infoDB :: Q Exp
infoDB = logDB LevelInfo

debugDB :: Q Exp
debugDB = logDB LevelDebug

reportErrorDB :: Q Exp
reportErrorDB = logDB LevelError

debugIO :: MonadIO m => Logger -> Loc -> String -> m ()
debugIO logger loc = logIO logger loc LevelDebug

infoIO :: MonadIO m => Logger -> Loc -> String -> m ()
infoIO logger loc = logIO logger loc LevelInfo

reportErrorIO :: MonadIO m => Logger -> Loc -> String -> m ()
reportErrorIO logger loc = logIO logger loc LevelError

getLoggingSettings :: GlobalConfig -> LogBackend
getLoggingSettings cfg =
    case lcTarget $ dbcLogging cfg of
      LogSyslog -> LogBackend $ defaultSyslogSettings {ssIdent = "batchd", ssFilter = fltr}
      LogStdout -> LogBackend $ defStdoutSettings {lsFilter = fltr}
      LogStderr -> LogBackend $ defStderrSettings {lsFilter = fltr}
      LogFile path -> LogBackend $ (defFileSettings path) {lsFilter = fltr}
  where
    fltr :: LogFilter
    fltr = map toFilter (lcFilter $ dbcLogging cfg) ++ [([], lcLevel $ dbcLogging cfg)]

    toFilter :: (String, LogLevel) -> (LogSource, LogLevel)
    toFilter (src, level) = (splitDots src, level)

