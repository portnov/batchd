{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Shell where

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

commandHandler :: Client ()
commandHandler = do
  opts <- gets (cmdCommand . csCmdline)
  case opts of
    Enqueue {} -> doEnqueue
    List {} -> doList
    Stats {} -> doStats
    Type {} -> doType
    Job {} -> do
      let mode = if jobMode opts == Delete
                    then Delete
                    else if jobMode opts == Update || isJust (status opts) || isJust (hostName opts) || isJust (queueName opts)
                          then Update
                          else View
      case mode of
        View -> viewJob
        Update -> updateJob
        Delete -> deleteJob
    Queue {} ->
      case queueMode opts of
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
      runShell

runShell :: Client ()
runShell = do
  mbLine <- liftIO $ readline "batch> "
  case mbLine of
    Nothing -> return ()
    Just "exit" -> return ()
    Just "quit" -> return ()
    Just line -> do
        liftIO $ addHistory line
        let res = execParserPure defaultPrefs parserInfo (words line)
        case res of
          (Success cmd) -> do
            modify $ \st -> st {csCmdline = cmd}
            commandHandler
          (Failure failure) -> do
            progn <- liftIO $ getProgName
            let (msg,_) = renderFailure failure progn
            liftIO $ putStrLn msg
          (CompletionInvoked _) -> do
            liftIO $ putStrLn "bash-completion is not supported in interactive shell."
        runShell

