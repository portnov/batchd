{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Semigroup ((<>))
import Options.Applicative
import Data.Text.Format.Heavy
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Types (runDaemon, forkDaemon, setupTranslations)
import qualified Batchd.Daemon.Logging as Log
import Batchd.Daemon.Database
import Batchd.Daemon.Manager as Manager
import Batchd.Daemon.Dispatcher as Dispatcher

parser :: Parser DaemonMode
parser =
  hsubparser
    (  command "both"       (info (pure Both) (progDesc "run both manager and dispatcher"))
    <> command "manager"    (info (pure Manager) (progDesc "run manager"))
    <> command "dispatcher" (info (pure Dispatcher) (progDesc "run dispatcher"))
    )
  <|> pure Both

parserInfo :: ParserInfo DaemonMode
parserInfo = info (parser <**> helper)
               (fullDesc
               <> header "batchd - the batchd toolset daemon server-side program"
               <> progDesc "process client requests and / or execute batch jobs" )

main :: IO ()
main = do
  cmd <- execParser parserInfo
  cfgR <- loadGlobalConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      let mode = if cmd == Both
                   then dbcDaemonMode cfg
                   else cmd
      let logSettings = Log.getLoggingSettings cfg
      runDaemon cfg Nothing logSettings $ do
        Batchd.Core.Daemon.Types.setupTranslations translationPolicy
        tr <- getTranslations
        $(Log.debug) "Loaded translations: {}" (Single $ show tr)
        $(Log.debug) "Loaded global configuration file: {}" (Single $ show cfg)
        connectPool
        case mode of
          Manager    -> Manager.runManager
          Dispatcher -> Dispatcher.runDispatcher
          Both -> do
            forkDaemon $ Manager.runManager
            Dispatcher.runDispatcher

