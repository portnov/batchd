{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Dispatcher (runDispatcher) where

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

import Batchd.Daemon.Types
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Common.Config as Config
import Batchd.Daemon.Database
import Batchd.Common.Schedule
import Batchd.Daemon.Schedule
import Batchd.Daemon.Executor
import Batchd.Daemon.Hosts (hostCleaner)
import Batchd.Core.Daemon.Logging
import Batchd.Core.Daemon.Hosts

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

  -- Listen for job results
  forkDaemon $ callbackListener resChan

  -- Stop hosts when they are not needed anymore
  withLogVariable "thread" ("host cleaner" :: String) $ do
      lts <- askLoggingStateM
      liftIO $ forkIO $ hostCleaner lts counters

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
                Nothing -> $debug "Queue `{}' exhaused." (Single qname)
                Just job -> do
                    -- Waiting means that Dispatcher saw job and put it to Chan to be
                    -- picked by some of workers.
                    setJobStatus job Waiting
                    liftIO $ writeChan jobsChan (entityVal qe, job)
        -- Sleep for poll timeout
        liftIO $ threadDelay $ (dbcPollTimeout cfg) * 1000*1000

-- | This listens for job results Chan and writes results to DB.
-- It also reschedules failed jobs if needed.
callbackListener :: ResultsChan -> Daemon ()
callbackListener resChan = withLogVariable "thread" ("job result listener" :: String) $ forever $ do
    (job, command) <- liftIO $ readChan resChan
    let jid = jiId job
    let jkey = JobKey (Sql.SqlBackendKey jid)
    now <- liftIO getCurrentTime
      
    withJobContext job $ do
      case command of
        StartExecution -> do
          -- let result = JobResult jkey now Nothing T.empty T.empty
          -- catchDbError $ runDB $ insert_ result
          return ()

        StdoutLine line -> do
          let result = JobResult jkey now Nothing line T.empty
          catchDbError $ runDB $ do
            insert_ result

        StderrLine line -> do
          let result = JobResult jkey now Nothing T.empty line
          catchDbError $ runDB $ do
            insert_ result

        ExecError msg onFail -> do
          let result = JobResult jkey now Nothing T.empty msg
          catchDbError $ runDB $ do
            insert_ result
            processFail job onFail

        Exited ec onFail -> do
          let result = JobResult jkey now (Just ec) T.empty T.empty
          catchDbError $ runDB $ do
            insert_ result
            if ec == ExitSuccess
              then setJobStatus job Done
              else processFail job onFail
  where
    catchDbError db = do
      res <- db
      case res of
        Right _ -> return ()
        Left err -> do
          $reportError "Can't update job result: {}" (Single $ show err)
          return ()

    processFail job Continue = setJobStatus job Failed -- just mark job as Failed
    processFail job (RetryNow m) = do
        count <- increaseTryCount job
        if count <= m
          then do
            $info "Retry now" ()
            setJobStatus job New -- job will be picked up by dispatcher at nearest iteration.
          else setJobStatus job Failed
    processFail job (RetryLater m) = do
        count <- increaseTryCount job
        if count <= m
          then do
            $info "Retry later" ()
            moveToEnd job -- put the job to the end of queue.
          else setJobStatus job Failed

withJobContext :: HasLogContext m => JobInfo -> m a -> m a
withJobContext job =
    withLogContext (LogContextFrame vars noChange)
  where
    vars = [("job", Variable (jiId job)),
            ("user", Variable (jiUserName job))]

-- | Worker loop executes jobs themeselves
worker :: Int -> HostsPool -> Chan (Queue, JobInfo) -> ResultsChan -> Daemon ()
worker idx hosts jobsChan resChan = forever $ withLogVariable "worker" idx $ do
  (queue, job) <- liftIO $ readChan jobsChan
  withJobContext job $ do
    $info "got job #{}" (Single $ jiId job)
    -- now job is picked up by worker, mark it as Processing
    runDB $ setJobStatus job Processing
    jtypeR <- liftIO $  Config.loadTemplate (jiType job)
    case jtypeR of
            Left err -> do -- we could not load job type description
                $reportError "Invalid job type {}: {}" (jiType job, show err)
                liftIO $ writeChan resChan (job, ExecError (T.pack $ show err) Continue)

            Right jtype -> do
                executeJob hosts queue jtype job resChan

    $info "done job #{}." (Single $ jiId job)

