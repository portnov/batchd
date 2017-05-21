{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Client.Shell (commandHandler) where

import Control.Exception
import Control.Monad.State
import Data.Maybe
import Options.Applicative
import System.Console.Readline
import System.Environment (getProgName)

import Client.Types
import Client.Actions
import Client.CmdLine
import Client.Config
import Client.Monad
import Client.Http

detectMode :: CrudMode -> [Command -> Bool] -> Command -> CrudMode
detectMode mode mods cmd =
  if mode == Delete
    then Delete
    else if mode == Add
           then Add
           else if mode == Update || or [mod cmd | mod <- mods]
                  then Update
                  else View

commandHandler :: Client ()
commandHandler = do
  opts <- gets (cmdCommand . csCmdline)
  case opts of
    Enqueue {} -> doEnqueue
    List {} -> doList
    Stats {} -> doStats
    Type {} -> doType
    Job {} -> do
      let mode = detectMode (jobMode opts) [isJust . status, isJust . hostName, isJust . queueName] opts
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
    User {} ->
      case userMode opts of
        View -> doListUsers
        Add -> doAddUser
        Update -> doChangePassword
    Grant {} ->
      case grantMode opts of
        View -> doListPermissions
        Add -> doAddPermission
    Shell {} -> do
      obtainCredentials
      liftIO $ putStrLn "This is batch client shell. Type `--help' for list of available commands or `some_command --help' (without quotes) for help message on particular command."
      runShell

errorHandler :: ClientException -> Client ()
errorHandler (ClientException e) =
  liftIO $ putStrLn $ "Error: " ++ e

runShell :: Client ()
runShell = do
  mbLine <- liftIO $ readline "batch> "
  case mbLine of
    Nothing -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just "" -> runShell
    Just line -> do
        liftIO $ addHistory line
        let res = execParserPure defaultPrefs parserInfo (words line)
        case res of
          (Success cmd) -> do
            modify $ \st -> st {csCmdline = cmd}
            commandHandler `catchC` errorHandler
          (Failure failure) -> do
            progn <- liftIO $ getProgName
            let (msg,_) = renderFailure failure progn
            liftIO $ putStrLn msg
          (CompletionInvoked _) -> do
            liftIO $ putStrLn "bash-completion is not supported in interactive shell."
        runShell

