{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad.Trans
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Database.Persist.Sql as Sql
import System.Log.Heavy

import Common.Config
import Common.Data (migrateAll)
import Daemon.Logging (getLoggingSettings)
import Daemon.Database
import Daemon.Auth

data Admin =
    CreateSuperuser {username :: String}
  | Migrate
  deriving (Show)

createSuperuser :: Parser Admin
createSuperuser = CreateSuperuser
  <$> strArgument (metavar "NAME" <> help "name of user to create" <> value "root" <> showDefault)

parser :: Parser Admin
parser =
  hsubparser
    (  command "create-superuser" (info createSuperuser (progDesc "create super user"))
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
            withLogging logSettings id $ do
              logger <- ask
              liftIO $ createSuperUser cfg logger (username cmd) password
            return ()
        Migrate -> do
          withLogging logSettings id $ do
              logger <- ask
              pool <- liftIO $ getPool cfg logger
              Sql.runSqlPool (Sql.runMigration migrateAll) pool

