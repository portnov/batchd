{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Client
import System.Console.CmdArgs

import Client.Types
import Client.Actions
import Client.CmdLine

main :: IO ()
main = do
  let mode = cmdArgsMode $ modes [enqueue, list &= name "ls", job, queue, schedule, typesList, stats]
  opts <- cmdArgsRun mode

  manager <- newManager defaultManagerSettings

  case opts of
    Enqueue {} -> doEnqueue manager opts
    List {} -> doList manager opts
    Stats {} -> doStats manager opts
    Type {} -> doType manager opts
    Job {} ->
      case jobMode opts of
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

