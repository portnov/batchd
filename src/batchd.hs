{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent
import System.Console.CmdArgs

import CommonTypes
import Config
import Database
import Manager
import Dispatcher

batchd :: DaemonMode
batchd =
  modes [Both &= auto, Manager, Dispatcher]
    &= program "batchd"

main :: IO ()
main = do
  cmd <- cmdArgs batchd
  cfgR <- loadGlobalConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      let mode = if cmd == Both
                   then dbcDaemonMode cfg
                   else cmd
      pool <- getPool cfg
      case mode of
        Manager    -> Manager.runManager cfg pool
        Dispatcher -> Dispatcher.runDispatcher cfg pool
        Both -> do
          forkIO $ Manager.runManager cfg pool
          Dispatcher.runDispatcher cfg pool

