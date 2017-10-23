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
import qualified Database.Persist.Sql as Sql hiding (Single)
import System.Exit
import Data.Text.Format.Heavy
import System.Log.Heavy

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
runDispatcher :: Daemon ()
runDispatcher = do
  cfg <- askConfig
  jobsChan <- liftIO newChan
  resChan <- liftIO newChan
  counters <- liftIO $ newMVar M.empty
  $info "Starting {} workers..." (Single $ dbcWorkers cfg)
  -- Each worker will run in separate thread.
  -- All workers read jobs to be executed from single Chan.
  -- So job put into Chan will be executed by first worker who sees it.
  forM_ [1.. dbcWorkers cfg] $ \idx -> do
    $debug "  Starting worker #{}" (Single idx)
    forkDaemon $ worker idx counters jobsChan resChan
  forkDaemon $ callbackListener resChan
  dispatcher jobsChan

-- | Dispatcher main loop itself.
dispatcher :: Chan (Queue, JobInfo) -> Daemon ()
dispatcher jobsChan = withLogVariable "thread" ("dispatcher" :: String) $ do
  forever $ do
    qesr <- runDB getEnabledQueues
    cfg <- askConfig
    case qesr of
      Left err -> $reportError "Can't get list of enabled queues: {}" (Single $ Shown err)
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
                Nothing -> $debugDB "Queue `{}' exhaused." (Single qname)
                Just job -> do
                    -- Waiting means that Dispatcher saw job and put it to Chan to be
                    -- picked by some of workers.
                    setJobStatus job Waiting
                    liftIO $ writeChan jobsChan (entityVal qe, job)
        -- Sleep for poll timeout
        liftIO $ threadDelay $ (dbcPollTimeout cfg) * 1000*1000

-- | This listens for job results Chan and writes results to DB.
-- It also reschedules failed jobs if needed.
callbackListener :: Chan (JobInfo, JobResult, OnFailAction) -> Daemon ()
callbackListener resChan = withLogVariable "thread" ("job result listener" :: String) $ forever $ do
  (job, result, onFail) <- liftIO $ readChan resChan
  withJobContext job $ do
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
                        $infoDB "Retry now" ()
                        setJobStatus job New -- job will be picked up by dispatcher at nearest iteration.
                      else setJobStatus job Failed
                 RetryLater m -> do
                    count <- increaseTryCount job
                    if count <= m
                      then do
                        $infoDB "Retry later" ()
                        moveToEnd job -- put the job to the end of queue.
                      else setJobStatus job Failed

withJobContext job =
    withLogContext (LogContextFrame vars noChange)
  where
    vars = [("job", Variable (jiId job)),
            ("user", Variable (jiUserName job))]

-- | Worker loop executes jobs themeselves
worker :: Int -> HostCounters -> Chan (Queue, JobInfo) -> Chan (JobInfo, JobResult, OnFailAction) -> Daemon ()
worker idx hosts jobsChan resChan = forever $ withLogVariable "worker" idx $ do
  (queue, job) <- liftIO $ readChan jobsChan
  withJobContext job $ do
    $info "got job #{}" (Single $ jiId job)
    -- now job is picked up by worker, mark it as Processing
    runDB $ setJobStatus job Processing
    jtypeR <- liftIO $  Config.loadTemplate (jiType job)
    (result, onFail) <-
        case jtypeR of
                Left err -> do -- we could not load job type description
                    $reportError "Invalid job type {}: {}" (jiType job, show err)
                    let jid = JobKey (Sql.SqlBackendKey $ jiId job)
                    now <- liftIO getCurrentTime
                    let res = JobResult jid now (ExitFailure (-1)) T.empty (T.pack $ show err)
                    return (res, Continue)

                Right jtype -> do
                    res <- executeJob hosts queue jtype job
                    return (res, jtOnFail jtype)

    -- put job result to Chan, to be picked up by callbacklistener
    liftIO $ writeChan resChan (job, result, onFail)
    $info "done job #{}." (Single $ jiId job)

