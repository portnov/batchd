{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics
import System.Console.CmdArgs

import Manager
import Dispatcher

data Batchd =
    Manager
  | Dispatcher
  deriving (Data, Typeable, Show, Eq)

manager :: Batchd
manager = Manager

dispatcher :: Batchd
dispatcher = Dispatcher

main :: IO ()
main = do
  cmd <- cmdArgs (modes [manager, dispatcher])
  case cmd of
    Manager -> Manager.runManager
    Dispatcher -> Dispatcher.runDispatcher

