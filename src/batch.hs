{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import System.Console.CmdArgs
import Control.Exception
import Data.Maybe

import Client.Types
import Client.Actions
import Client.CmdLine
import Client.Config
import Client.Monad
import Client.Http

main :: IO ()
main = realMain `catch` errorHandler

errorHandler :: ClientException -> IO ()
errorHandler (ClientException e) = putStrLn $ "Error: " ++ e

realMain :: IO ()
realMain = do
  let mode = cmdArgsMode $ modes [enqueue, list &= name "ls", job, queue, schedule, typesList, stats, user, grant]
  opts <- cmdArgsRun mode

  manager <- makeClientManager opts
  cfg <- loadClientConfig
  let state = ClientState opts cfg Nothing Nothing manager

  runClient state $
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

