{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Exception
import Data.Maybe

import Client.Types
import Client.Actions
import Client.CmdLine
import Client.Config
import Client.Monad
import Client.Http
import Client.Shell

main :: IO ()
main = realMain `catch` errorHandler

errorHandler :: ClientException -> IO ()
errorHandler (ClientException e) = putStrLn $ "Error: " ++ e

realMain :: IO ()
realMain = do
  opts <- getCmdArgs
  print opts

  manager <- makeClientManager opts
  cfg <- loadClientConfig
  let state = ClientState opts cfg Nothing Nothing manager

  runClient state $ commandHandler

