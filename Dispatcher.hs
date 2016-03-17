{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dispatcher (runDispatcher) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.ByteString as B
import qualified Data.Map as M
-- import Data.Aeson
import Data.Default
import Database.Persist
import qualified Database.Persist.Sql as Sql
import qualified Database.Persist.Sqlite as Sqlite
import System.Environment
import Text.Printf

import Database
import Types
import Schedule

runDispatcher :: IO ()
runDispatcher = do
  pool <- getPool
  Sql.runSqlPool (Sql.runMigration migrateAll) pool
  runReaderT (runConnection dispatcher) pool

dispatcher :: ConnectionM ()
dispatcher = do
  forever $ do
    qesr <- runDB getAllQueues
    case qesr of
      Left err -> liftIO $ print err
      Right qes -> do
        forM_ qes $ \qe -> runDB $ do
          let QueueKey qname = entityKey qe
          mbJob <- getNextJob (entityKey qe)
          case mbJob of
            Nothing -> liftIO $ print $ "Queue " ++ qname ++ " exhaused."
            Just job -> process job
          liftIO $ threadDelay $ 10 * 1000*1000

process :: JobInfo -> DB ()
process job = do
  liftIO $ print job
  setJobStatus job Processing
  liftIO $ threadDelay $ 1 * 1000*1000
  setJobStatus job Done


