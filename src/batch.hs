{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Control.Monad
import Control.Exception
import qualified Data.Aeson as Aeson
import Data.Yaml
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Maybe
import Data.List (intercalate)
import Data.Char (toLower)
import Data.Generics hiding (Generic)
import Data.Dates
import Network.HTTP.Client
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status
import System.Console.CmdArgs
import System.FilePath
import System.Environment (lookupEnv)
import Text.Printf

import Common.CommonTypes
import qualified Common.Data as Database
import Common.Schedule
import Common.Config
import Client.Types
import Client.Config
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

