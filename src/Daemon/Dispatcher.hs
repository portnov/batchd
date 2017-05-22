{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Daemon.Dispatcher (runDispatcher) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Time
import Data.Dates
import Database.Persist
import qualified Database.Persist.Sql as Sql
import System.Exit
import Text.Printf

import Common.Types
import Daemon.Types
import Common.Config as Config
import Common.Data
import Daemon.Database
import Common.Schedule
import Daemon.Schedule
import Daemon.Executor
import Daemon.Logging
import Daemon.Hosts

-- | Set up workers, callback listener and dispatcher itself.
runDispatcher :: GlobalConfig -> Sql.ConnectionPool -> IO ()
runDispatcher cfg pool = do
  let connInfo = ConnectionInfo cfg pool
  jobsChan <- newChan
  resChan <- newChan
  counters <- newMVar M.empty
  $infoDB cfg $ "Starting " ++ show (dbcWorkers cfg) ++ " workers"
  -- Each worker will run in separate thread.
  -- All workers read jobs to be executed from single Chan.
  -- So job put into Chan will be executed by first worker who sees it.
  forM_ [1.. dbcWorkers cfg] $ \idx -> do
    $debugDB cfg $ "  Starting worker #" ++ show idx
    forkIO $ worker cfg pool idx counters jobsChan resChan
  forkIO $ runReaderT (runConnection (callbackListener resChan)) connInfo
  runReaderT (runConnection (dispatcher jobsChan)) connInfo

-- | Dispatcher main loop itself.
dispatcher :: Chan (Queue, JobInfo) -> ConnectionM ()
dispatcher jobsChan = do
  forever $ do
    qesr <- runDB getEnabledQueues
    cfg <- asks ciGlobalConfig
    case qesr of
      Left err -> $reportError (show err) -- database exception
      Right qes -> do
        forM_ qes $ \qe -> runDB $ do
          schedule <- loadSchedule (queueSchedule $ entityVal qe)
          now <- liftIO $ getCurrentDateTime
          when (schedule `allows` now) $ do
              -- will work only with queues, shedules of which allow current time
              let QueueKey qname = entityKey qe
              -- pick next job from the queue
              mbJob <- getNextJob (entityKey qe)
              case mbJob of
                Nothing -> $debugDB cfg $ "Queue " ++ qname ++ " exhaused."
                Just job -> do
                    -- Waiting means that Dispatcher saw job and put it to Chan to be
                    -- picked by some of workers.
                    setJobStatus job Waiting
                    liftIO $ writeChan jobsChan (entityVal qe, job)
        -- Sleep for poll timeout
        liftIO $ threadDelay $ (dbcPollTimeout cfg) * 1000*1000

-- | This listens for job results Chan and writes results to DB.
-- It also reschedules failed jobs if needed.
callbackListener :: Chan (JobInfo, JobResult, OnFailAction) -> ConnectionM ()
callbackListener resChan = forever $ do
  (job, result, onFail) <- liftIO $ readChan resChan
  cfg <- asks ciGlobalConfig
  runDB $ do
      insert_ result
      if jobResultExitCode result == ExitSuccess
        then setJobStatus job Done
        else case onFail of
               Continue -> setJobStatus job Failed -- just mark job as Failed
               RetryNow m -> do
                  count <- increaseTryCount job
                  if count <= m
                    then do
                      $infoDB cfg "Retry now"
                      setJobStatus job New -- job will be picked up by dispatcher at nearest iteration.
                    else setJobStatus job Failed
               RetryLater m -> do
                  count <- increaseTryCount job
                  if count <= m
                    then do
                      $infoDB cfg "Retry later"
                      moveToEnd job -- put the job to the end of queue.
                    else setJobStatus job Failed

-- | Worker loop executes jobs themeselves
worker :: GlobalConfig -> Sql.ConnectionPool -> Int -> HostCounters -> Chan (Queue, JobInfo) -> Chan (JobInfo, JobResult, OnFailAction) -> IO ()
worker cfg pool idx hosts jobsChan resChan = forever $ do
  (queue, job) <- readChan jobsChan
  $infoDB cfg $ printf "[%d] got job #%d" idx (jiId job)
  -- now job is picked up by worker, mark it as Processing
  runDBIO cfg pool $ setJobStatus job Processing
  jtypeR <- Config.loadTemplate (jiType job)
  (result, onFail) <-
      case jtypeR of
              Left err -> do -- we could not load job type description
                  $reportErrorDB cfg $ printf "[%d] invalid job type %s: %s" idx (jiType job) (show err)
                  let jid = JobKey (Sql.SqlBackendKey $ jiId job)
                  now <- getCurrentTime
                  let res = JobResult jid now (ExitFailure (-1)) T.empty (T.pack $ show err)
                  return (res, Continue)

              Right jtype -> do
                  res <- executeJob cfg hosts queue jtype job
                  return (res, jtOnFail jtype)

  -- put job result to Chan, to be picked up by callbacklistener
  writeChan resChan (job, result, onFail)
  $infoDB cfg $ printf "[%d] done job #%d" idx (jiId job)

