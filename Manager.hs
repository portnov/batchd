{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Manager where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class  (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Resource
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import qualified Data.Map as M
-- import Data.Aeson
import Data.Default
import Database.Persist
import qualified Database.Persist.Sql as Sql
import qualified Database.Persist.Sqlite as Sqlite
import System.Environment
import Text.Printf
-- import Web.Scotty
import Web.Scotty.Trans as Scotty

import Database
import Types
import Schedule

runApplication :: Sql.ConnectionPool -> IO ()
runApplication pool = do
  let options = def
  let r m = runReaderT (runConnection m) pool
  scottyOptsT options r application

application :: ScottyT Error ConnectionM ()
application = do
  -- runDB (Sql.runMigration migrateAll)
  Scotty.get "/queues" getQueuesA
  Scotty.get "/queue/:name" getQueueA

manager :: IO ()
manager = do
  pool <- getPool
  Sql.runSqlPool (Sql.runMigration migrateAll) pool
  runApplication pool

getQueuesA :: Action ()
getQueuesA = do
  qes <- runDB getAllQueues
  let qnames = map (queueName . entityVal) qes
  Scotty.json qnames

getQueueA :: Action ()
getQueueA = do
  qname <- Scotty.param "name"
  jobs <- runDB $ loadJobs qname (Just New)
  Scotty.json jobs


