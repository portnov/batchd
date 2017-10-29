
module Main where

import System.Log.Heavy
import System.Log.Heavy.IO
import Batchd.Core.Common.Types
import Batchd.Daemon.Hosts

main :: IO ()
main = withLoggingIO (LoggingSettings defStderrSettings) $ do
  logger <- getLogger
  b <- getLogBackend
  let lts = LoggingTState logger b []
  c <- loadHostController lts "amazon"
  print c
