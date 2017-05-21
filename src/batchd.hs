{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent
import Data.Semigroup ((<>))
import Options.Applicative

import Common.Types
import Common.Config
import Daemon.Database
import Daemon.Manager as Manager
import Daemon.Dispatcher as Dispatcher

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
      pool <- getPool cfg
      case mode of
        Manager    -> Manager.runManager cfg pool
        Dispatcher -> Dispatcher.runDispatcher cfg pool
        Both -> do
          forkIO $ Manager.runManager cfg pool
          Dispatcher.runDispatcher cfg pool

