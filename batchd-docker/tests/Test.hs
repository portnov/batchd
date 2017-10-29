
module Main where

import System.Environment
import Batchd.Core
import Batchd.Ext.Docker

main :: IO ()
main = do
  [action, id] <- getArgs
  let docker = Docker True (Just "/var/run/docker.sock") defaultDockerUrl
  case action of
    "start" -> startHost docker id
    "stop"  -> stopHost docker id
    _ -> fail $ "unknown action"

