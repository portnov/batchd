{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.AWS
import System.Environment
import Batchd.Core
import Batchd.Ext.AWS
import System.Log.Heavy
import System.Log.Heavy.IO

main :: IO ()
main = withLoggingIO (LoggingSettings defStderrSettings) $ do
  [action, region, id] <- getArgs
  logger <- getLogger
  let credentials = FromFile "batchd" "credentials"
  let aws = AWSEC2 True credentials (read region) logger
  case action of
    "start" -> startHost aws id
    "stop"  -> stopHost  aws id
    _ -> fail "unknown action"

