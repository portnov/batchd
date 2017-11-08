{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Batchd.Daemon.CmdLine
  (CommonOpts (..),
   DaemonCmdLine (..),
   Admin (..), AdminCmdLine (..),
   daemonParserInfo,
   adminParserInfo
  ) where

import Data.Semigroup ((<>))
import Options.Applicative

import Batchd.Common.Types

data CommonOpts = CommonOpts {
      globalConfigPath :: Maybe FilePath
    }
    deriving (Eq, Show)

data DaemonCmdLine = DaemonCmdLine {
      daemonCommon :: CommonOpts
    , daemonMode :: DaemonMode
    }
    deriving (Eq, Show)

data AdminCmdLine = AdminCmdLine {
      adminCommon :: CommonOpts
    , adminCommand :: Admin
    }
    deriving (Eq, Show)

data Admin =
    CreateSuperuser {username :: String}
  | Passwd {username :: String}
  | Migrate
  deriving (Show, Eq)

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
  <$> optional (strOption
      ( long "config"
      <> short 'c'
      <> metavar "PATH"
      <> help "path to global configuration file (batchd.yaml)"))

pDaemonMode :: Parser DaemonMode
pDaemonMode =
  hsubparser
    (  command "both"       (info (pure Both) (progDesc "run both manager and dispatcher"))
    <> command "manager"    (info (pure Manager) (progDesc "run manager"))
    <> command "dispatcher" (info (pure Dispatcher) (progDesc "run dispatcher"))
    )
  <|> pure Both

daemonParser :: Parser DaemonCmdLine
daemonParser = DaemonCmdLine <$> commonOpts <*> pDaemonMode

daemonParserInfo :: ParserInfo DaemonCmdLine
daemonParserInfo = info (daemonParser <**> helper)
               (fullDesc
               <> header "batchd - the batchd toolset daemon server-side program"
               <> progDesc "process client requests and / or execute batch jobs" )

createSuperuser :: Parser Admin
createSuperuser = CreateSuperuser
  <$> strArgument (metavar "NAME" <> help "name of user to create" <> value "root" <> showDefault)

passwd :: Parser Admin
passwd = Passwd <$> strArgument (metavar "NAME" <> help "name of user to update" <> value "root" <> showDefault)

pAdminCommand :: Parser Admin
pAdminCommand =
  hsubparser
    (  command "create-superuser" (info createSuperuser (progDesc "create super user"))
    <> command "passwd" (info passwd (progDesc "change user password"))
    <> command "upgrade-db" (info (pure Migrate) (progDesc "upgrade database to current version of batchd")))

adminParser :: Parser AdminCmdLine
adminParser = AdminCmdLine <$> commonOpts <*> pAdminCommand

adminParserInfo :: ParserInfo AdminCmdLine
adminParserInfo = info (adminParser <**> helper)
               (fullDesc
               <> header "batchd-admin - the batchd toolset administrative utility")

