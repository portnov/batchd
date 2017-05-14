{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Client
import System.Console.CmdArgs
import Control.Exception
import Data.Maybe

import Client.Types
import Client.Actions
import Client.CmdLine

main :: IO ()
main = realMain `catch` errorHandler

errorHandler :: ClientException -> IO ()
errorHandler (ClientException e) = putStrLn $ "Error: " ++ e

realMain :: IO ()
realMain = do
  let mode = cmdArgsMode $ modes [enqueue, list &= name "ls", job, queue, schedule, typesList, stats]
  opts <- cmdArgsRun mode

  manager <- newManager defaultManagerSettings

  case opts of
    Enqueue {} -> doEnqueue manager opts
    List {} -> doList manager opts
    Stats {} -> doStats manager opts
    Type {} -> doType manager opts
    Job {} -> do
      let mode = if jobMode opts == Delete
                    then Delete
                    else if jobMode opts == Update || isJust (status opts) || isJust (hostName opts) || isJust (queueName opts)
                          then Update
                          else View
      case mode of
        View -> viewJob manager opts
        Update -> updateJob manager opts
        Delete -> deleteJob manager opts
    Queue {} ->
      case queueMode opts of
        Add -> addQueue manager opts
        Update -> updateQueue manager opts
        Delete -> deleteQueue manager opts
    Schedule {} -> 
      case scheduleMode opts of
        View -> doListSchedules manager opts
        Add -> doAddSchedule manager opts
        Delete -> doDeleteSchedule manager opts

