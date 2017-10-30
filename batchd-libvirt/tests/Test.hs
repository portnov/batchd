
module Main where

import System.Environment
import Batchd.Core
import System.Log.Heavy
import System.Log.Heavy.IO
import qualified System.Log.FastLogger as F
import Batchd.Ext.LibVirt

force action = do
  r <- action
  case r of
    Right _ -> return ()
    Left err -> fail $ show err

settings = defStderrSettings {lsType = F.LogStderr 0}

main :: IO ()
main = withLoggingIO (LoggingSettings settings) $ do
  [uri, action, id] <- getArgs
  logger <- getLogger
  backend <- getLogBackend
  let libvirt = LibVirt True uri $ LoggingTState logger backend []
  case action of
    "start" -> force $ startHost libvirt id
    "stop"  -> force $ stopHost libvirt id
    _ -> fail $ "unknown action"

