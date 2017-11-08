{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

import Options.Applicative
import Data.Text.Format.Heavy hiding (optional)
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import qualified Batchd.Core.Daemon.Logging as Log
import Batchd.Daemon.Types (runDaemon, forkDaemon, setupTranslations)
import qualified Batchd.Daemon.Logging as Log
import Batchd.Common.Config
import Batchd.Common.Types
import Batchd.Daemon.Database
import Batchd.Daemon.Monitoring
import Batchd.Daemon.CmdLine
import Batchd.Daemon.Manager as Manager
import Batchd.Daemon.Dispatcher as Dispatcher

main :: IO ()
main = do
  cmdline <- execParser daemonParserInfo
  let cmd = daemonMode cmdline
  cfgR <- loadGlobalConfig (globalConfigPath $ daemonCommon cmdline)
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      let cfg' = if cmd == Both
                   then cfg
                   else cfg {dbcDaemonMode = cmd}
      let mode = dbcDaemonMode cfg'
      let logSettings = Log.getLoggingSettings cfg'
      runDaemon cfg' Nothing logSettings $ do
        Batchd.Daemon.Types.setupTranslations translationPolicy
        tr <- getTranslations
        $(Log.putMessage config_level) "Loaded translations: {}" (Single $ show tr)
        case globalConfigPath $ daemonCommon cmdline of
          Nothing -> return ()
          Just path -> $(Log.putMessage config_level) "Using global configuration file: {}" (Single path)
        $(Log.putMessage config_level) "Loaded global configuration file: {}" (Single $ show cfg')
        connectPool
        setupMetrics
        case mode of
          Manager    -> Manager.runManager
          Dispatcher -> Dispatcher.runDispatcher
          Both -> do
            forkDaemon $ Manager.runManager
            Dispatcher.runDispatcher

