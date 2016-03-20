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
import Data.Yaml
import Data.Default
import Data.Dates
import Database.Persist
import qualified Database.Persist.Sql as Sql
import qualified Database.Persist.Sqlite as Sqlite
import System.Environment
import System.Exit
import Text.Printf

import CommonTypes
import Types
import Config
import Database
import Schedule
import Executor

runDispatcher :: IO ()
runDispatcher = do
  cfgR <- loadDbConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      pool <- getPool cfg
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
          schedule <- loadSchedule (queueSchedule $ entityVal qe)
          now <- liftIO $ getCurrentDateTime
          when (schedule `allows` now) $ do
              let QueueKey qname = entityKey qe
              mbJob <- getNextJob (entityKey qe)
              case mbJob of
                Nothing -> liftIO $ print $ "Queue " ++ qname ++ " exhaused."
                Just job -> process (entityVal qe) job
        liftIO $ threadDelay $ 10 * 1000*1000

loadTemplateDb :: String -> DB JobType
loadTemplateDb name = do
  r <- liftIO $ Config.loadTemplate name
  case r of
    Left err -> throwR err
    Right jt -> return jt

process :: Queue -> JobInfo -> DB ()
process queue job = do
  liftIO $ print job
  lockJob job
  setJobStatus job Processing
  jtype <- loadTemplateDb (jiType job)
  result <- liftIO $ executeJob queue jtype job
  insert_ result
  if jobResultExitCode result == ExitSuccess
    then setJobStatus job Done
    else case jtOnFail jtype of
           Continue -> setJobStatus job Failed
           RetryNow m -> do
              count <- increaseTryCount job
              if count <= m
                then do
                  liftIO $ putStrLn "Retry now"
                  setJobStatus job New
                else setJobStatus job Failed
           RetryLater m -> do
              count <- increaseTryCount job
              if count <= m
                then do
                  liftIO $ putStrLn "Retry later"
                  moveToEnd job
                else setJobStatus job Failed
              
  


