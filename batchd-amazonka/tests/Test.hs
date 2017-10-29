{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.AWS
import System.Environment
import Batchd.Core
import Batchd.Ext.AWS
import System.Log.Heavy
import System.Log.Heavy.IO

force action = do
  r <- action
  case r of
    Right _ -> return ()
    Left err -> fail $ show err

main :: IO ()
main = withLoggingIO (LoggingSettings defStderrSettings) $ do
  [action, region, id] <- getArgs
  logger <- getLogger
  backend <- getLogBackend
  let credentials = FromFile "batchd" "credentials"
  let aws = AWSEC2 True credentials (read region) $ LoggingTState logger backend []
  case action of
    "start" -> force $ startHost aws id
    "stop"  -> force $ stopHost  aws id
    "name" -> do
      res <- getActualHostName aws id
      print res
    _ -> fail "unknown action"

