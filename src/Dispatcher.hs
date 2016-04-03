{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Dispatcher (runDispatcher) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Time
import Data.Dates
import Database.Persist
import qualified Database.Persist.Sql as Sql
import System.Exit
import Text.Printf

import CommonTypes
import Types
import Config
import Database
import Schedule
import Executor
import Logging

runDispatcher :: GlobalConfig -> Sql.ConnectionPool -> IO ()
runDispatcher cfg pool = do
  let connInfo = ConnectionInfo cfg pool
  -- Sql.runSqlPool (Sql.runMigration migrateAll) (ciPool connInfo)
  jobsChan <- newChan
  resChan <- newChan
  forM_ [1.. dbcWorkers cfg] $ \idx ->
    forkIO $ worker cfg idx jobsChan resChan
  forkIO $ runReaderT (runConnection (callbackListener resChan)) connInfo
  runReaderT (runConnection (dispatcher jobsChan)) connInfo

dispatcher :: Chan (Queue, JobInfo) -> ConnectionM ()
dispatcher jobsChan = do
  forever $ do
    qesr <- runDB getAllQueues
    cfg <- asks ciGlobalConfig
    case qesr of
      Left err -> $reportError (show err)
      Right qes -> do
        forM_ qes $ \qe -> runDB $ do
          schedule <- loadSchedule (queueSchedule $ entityVal qe)
          now <- liftIO $ getCurrentDateTime
          when (schedule `allows` now) $ do
              let QueueKey qname = entityKey qe
              mbJob <- getNextJob (entityKey qe)
              case mbJob of
                Nothing -> $debugDB cfg $ "Queue " ++ qname ++ " exhaused."
                Just job -> do
                    setJobStatus job Processing
                    liftIO $ writeChan jobsChan (entityVal qe, job)
        liftIO $ threadDelay $ (dbcPollTimeout cfg) * 1000*1000

callbackListener :: Chan (JobInfo, JobResult, OnFailAction) -> ConnectionM ()
callbackListener resChan = forever $ do
  (job, result, onFail) <- liftIO $ readChan resChan
  cfg <- asks ciGlobalConfig
  runDB $ do
      insert_ result
      if jobResultExitCode result == ExitSuccess
        then setJobStatus job Done
        else case onFail of
               Continue -> setJobStatus job Failed
               RetryNow m -> do
                  count <- increaseTryCount job
                  if count <= m
                    then do
                      $infoDB cfg "Retry now"
                      setJobStatus job New
                    else setJobStatus job Failed
               RetryLater m -> do
                  count <- increaseTryCount job
                  if count <= m
                    then do
                      $infoDB cfg "Retry later"
                      moveToEnd job
                    else setJobStatus job Failed


worker :: GlobalConfig -> Int -> Chan (Queue, JobInfo) -> Chan (JobInfo, JobResult, OnFailAction) -> IO ()
worker cfg idx jobsChan resChan = forever $ do
  (queue, job) <- readChan jobsChan
  $infoDB cfg $ printf "[%d] got job #%d" idx (jiId job)
  jtypeR <- Config.loadTemplate (jiType job)
  (result, onFail) <-
      case jtypeR of
              Left err -> do
                  $reportErrorDB cfg $ printf "[%d] invalid job type %s: %s" idx (jiType job) (show err)
                  let jid = JobKey (Sql.SqlBackendKey $ jiId job)
                  now <- getCurrentTime
                  let res = JobResult jid now (ExitFailure (-1)) T.empty (T.pack $ show err)
                  return (res, Continue)

              Right jtype -> do
                  res <- executeJob cfg queue jtype job
                  return (res, jtOnFail jtype)

  writeChan resChan (job, result, onFail)
  $infoDB cfg $ printf "[%d] done job #%d" idx (jiId job)

