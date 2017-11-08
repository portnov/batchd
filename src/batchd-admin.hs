{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad.Trans
import Control.Monad.Reader
import Options.Applicative
import qualified Database.Persist.Sql as Sql
import System.Log.Heavy

import Batchd.Common.Config
import Batchd.Common.Data (migrateAll)
import Batchd.Daemon.Logging (getLoggingSettings)
import Batchd.Daemon.Database
import Batchd.Daemon.Auth
import Batchd.Daemon.CmdLine

main :: IO ()
main = do
  cmdline <- execParser adminParserInfo
  cfgR <- loadGlobalConfig (globalConfigPath $ adminCommon cmdline)
  let cmd = adminCommand cmdline
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      let logSettings = getLoggingSettings cfg
      case cmd of
        CreateSuperuser {} -> do
            password <- getPassword2
            withLoggingT logSettings $ do
              logger <- ask
              liftIO $ createSuperUser cfg logger (username cmd) password
            return ()
        Passwd name -> do
            password <- getPassword2
            withLoggingT logSettings $ do
              logger <- ask
              liftIO $ changePassword cfg logger name password
            return ()
        Migrate -> do
          withLoggingT logSettings $ do
              logger <- ask
              pool <- liftIO $ getPool cfg logger
              Sql.runSqlPool (Sql.runMigration migrateAll) pool

