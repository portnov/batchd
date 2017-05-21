{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Semigroup ((<>))
import Options.Applicative

import Common.Types
import Common.Config
import Daemon.Database
import Daemon.Auth

data Admin =
  CreateSuperuser {userName :: String}
  deriving (Show)

createSuperuser :: Parser Admin
createSuperuser = CreateSuperuser
  <$> strArgument (metavar "NAME" <> help "name of user to create" <> value "root" <> showDefault)

parser :: Parser Admin
parser =
  hsubparser
    (command "create-superuser" (info createSuperuser (progDesc "create super user")))

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
      password <- getPassword2
      createSuperUser cfg (userName cmd) password
      return ()

