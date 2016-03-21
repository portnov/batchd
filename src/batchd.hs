{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Concurrent
import Data.Generics
import System.Console.CmdArgs

import CommonTypes
import Config
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
      case mode of
        Manager    -> Manager.runManager cfg
        Dispatcher -> Dispatcher.runDispatcher cfg
        Both -> do
          forkIO $ Manager.runManager cfg
          Dispatcher.runDispatcher cfg

