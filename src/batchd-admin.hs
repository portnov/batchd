{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Semigroup ((<>))
import Options.Applicative
import Database.Persist
import qualified Database.Persist.Sql as Sql

import Common.Types
import Common.Config
import Common.Data (migrateAll)
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
      case cmd of
        CreateSuperuser {} -> do
            password <- getPassword2
            createSuperUser cfg (username cmd) password
            return ()
        Migrate -> do
          pool <- getPool cfg
          Sql.runSqlPool (Sql.runMigration migrateAll) pool

