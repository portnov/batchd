{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.Map as M
-- import Data.Aeson
import Data.Generics
import Data.Default
import Database.Persist
import qualified Database.Persist.Sql as Sql
import qualified Database.Persist.Sqlite as Sqlite
import System.Environment
import System.Console.CmdArgs
import Text.Printf
-- import Web.Scotty
import Web.Scotty.Trans as Scotty

import Database
import Types
import Schedule
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

