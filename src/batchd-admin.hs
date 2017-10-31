{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad.Trans
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Database.Persist.Sql as Sql
import System.Log.Heavy

import Batchd.Core.Common.Config
import Batchd.Common.Config
import Batchd.Common.Data (migrateAll)
import Batchd.Daemon.Logging (getLoggingSettings)
import Batchd.Daemon.Database
import Batchd.Daemon.Auth

data Admin =
    CreateSuperuser {username :: String}
  | Passwd {username :: String}
  | Migrate
  deriving (Show)

createSuperuser :: Parser Admin
createSuperuser = CreateSuperuser
  <$> strArgument (metavar "NAME" <> help "name of user to create" <> value "root" <> showDefault)

passwd :: Parser Admin
passwd = Passwd <$> strArgument (metavar "NAME" <> help "name of user to update" <> value "root" <> showDefault)

parser :: Parser Admin
parser =
  hsubparser
    (  command "create-superuser" (info createSuperuser (progDesc "create super user"))
    <> command "passwd" (info passwd (progDesc "change user password"))
    <> command "upgrade-db" (info (pure Migrate) (progDesc "upgrade database to current version of batchd")))

parserInfo :: ParserInfo Admin
parserInfo = info (parser <**> helper)
               (fullDesc
               <> header "batchd-admin - the batchd toolset administrative utility")

main :: IO ()
main = do
  cmd <- execParser parserInfo
  cfgR <- loadGlobalConfig
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

