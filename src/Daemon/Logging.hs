{-# LANGUAGE TemplateHaskell #-}

module Daemon.Logging where

import Control.Monad.Reader hiding (lift)
import Control.Monad.Logger
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lift

import Common.Types
import Daemon.Types

deriveLift ''DaemonMode

deriveLift ''DbDriver

deriveLift ''AuthMode
deriveLift ''GlobalConfig

logConnectionM :: LogLevel -> Q Exp
logConnectionM level = [| \msg ->
  do
    cfg <- asks ciGlobalConfig
    let loc = $(qLocation >>= liftLoc)
    let src = T.pack (loc_module loc)
    enableLogging cfg ( monadLoggerLog loc src $(lift level) (T.pack msg) ) |]

debug :: Q Exp
debug = logConnectionM LevelDebug

info :: Q Exp
info = logConnectionM LevelInfo

reportError :: Q Exp
reportError = logConnectionM LevelError

logDB :: LogLevel -> Q Exp
logDB level = [| \cfg msg ->
  do
    let loc = $(qLocation >>= liftLoc)
    let src = T.pack (loc_module loc)
    enableLogging cfg ( monadLoggerLog loc src $(lift level) (T.pack msg) ) |]

infoDB :: Q Exp
infoDB = logDB LevelInfo

debugDB :: Q Exp
debugDB = logDB LevelDebug

reportErrorDB :: Q Exp
reportErrorDB = logDB LevelError

debugIO :: GlobalConfig -> String -> IO ()
debugIO cfg msg = do
  enableLogging cfg ( $(logDebug) $ T.pack msg ) 

infoIO :: GlobalConfig -> String -> IO ()
infoIO cfg msg = do
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

reportErrorIO :: GlobalConfig -> String -> IO ()
reportErrorIO cfg msg = do
  enableLogging cfg ( $(logError) $ T.pack msg ) 

