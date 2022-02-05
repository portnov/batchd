{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
-- | This module contains definitions for client interactive shell implementation.
module Batchd.Client.Shell (commandHandler) where

import Control.Monad.State
import Data.Maybe
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy
import Options.Applicative
import System.Console.Haskeline
import System.Environment (getProgName)

import Batchd.Core.Common.Localize
import Batchd.Client.Types
import Batchd.Client.Logging
import Batchd.Client.Actions
import Batchd.Client.CmdLine
import Batchd.Client.Monad
import Batchd.Client.Http

detectMode :: CrudMode -> [Command -> Bool] -> Command -> CrudMode
detectMode mode mods cmd =
  if mode == Delete
    then Delete
    else if mode == Add
           then Add
           else if mode == Update || or [mod cmd | mod <- mods]
                  then Update
                  else View

-- | Handle client command line.
commandHandler :: Client ()
commandHandler = do
  opts <- gets (cmdCommand . csCmdline)
  case opts of
    Enqueue {} -> doEnqueue
    List {} -> doList
    Stats {} -> doStats
    Type {} -> doType
    Hosts -> doHosts
    Job {} -> do
      let mode = detectMode (jobMode opts) [isJust . status, isJust . hostName, isJust . jobNotes, isJust . startTime, isJust . queueName, isJust . prioritize] opts
      case mode of
        View -> viewJob
        Update -> updateJob
        Delete -> deleteJob
    Queue {} -> do
      let mode = detectMode (queueMode opts) [isJust . scheduleName, isJust . hostName, isJust . title, isJust . enabled] opts
      case mode of
        Add -> addQueue
        Update -> updateQueue
        Delete -> deleteQueue
    Schedule {} -> 
      case scheduleMode opts of
        View -> doListSchedules
        Add -> doAddSchedule
        Delete -> doDeleteSchedule
    Monitor {} -> doMonitor
    User {} ->
      case userMode opts of
        View -> doListUsers
        Add -> doAddUser
        Update -> doChangePassword
    Grant {} ->
      case grantMode opts of
        View -> doListPermissions
        Add -> doAddPermission
        Delete -> doRevokePermission
    Shell {} -> do
      obtainCredentials
      message (__ "This is batch client shell. Type `--help' for list of available commands or `some_command --help' (without quotes) for help message on particular command.") ()
      runShell

errorHandler :: ClientException -> Client ()
errorHandler (ClientException e) =
  liftIO $ TLIO.putStrLn =<< (__f "Error: {}" (Single e))

-- | Interactive shell REPL.
runShell :: Client ()
runShell = runInputT defaultSettings loop
  where
    loop = do
      mbLine <- getInputLine "batch> "
      case mbLine of
        Nothing -> return ()
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just "" -> loop
        Just line -> do
            let res = execParserPure Options.Applicative.defaultPrefs parserInfo (words line)
            case res of
              (Success cmd) -> lift $ do
                modify $ \st -> st {csCmdline = cmd}
                commandHandler `catchC` errorHandler
              (Failure failure) -> do
                progn <- liftIO $ getProgName
                let (msg,_) = renderFailure failure progn
                liftIO $ putStrLn msg
              (CompletionInvoked _) -> do
                liftIO $ putStrLn "bash-completion is not supported in interactive shell."
            loop

