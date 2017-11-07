{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Executor where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary (sourceHandle)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse.Shell
import qualified Database.Persist.Sql as Sql hiding (Single)
import Data.Time
import System.Process
import System.FilePath
import System.IO

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Daemon.SSH
import Batchd.Daemon.Monitoring as Monitoring

getCommand :: Maybe Host -> JobType -> JobInfo -> String
getCommand mbHost jt job =
    TL.unpack $ format (parseShellFormat' $ TL.pack $ jtTemplate jt) (mkContext $ hostContext mbHost jt $ jiParams job)
  where
    mkContext m = optional $ map go $ M.assocs m :: ThenCheck [(TL.Text, String)] DefaultValue
    go (key, value) = (TL.pack key, value)

getHostName :: Queue -> JobType -> JobInfo -> Maybe String
getHostName q jt job =
  msum [jiHostName job, jtHostName jt, queueHostName q]

hostContext :: Maybe Host -> JobType -> JobParamInfo -> JobParamInfo
hostContext Nothing jt params = params
hostContext (Just host) jt params = M.fromList $ map update $ M.assocs params
  where
    update (key, value) =
      case getParamType jt key of
        Just InputFile -> (key, hInputDirectory host </> takeFileName value)
        Just OutputFile -> (key, hOutputDirectory host </> takeFileName value)
        _ -> (key, value)

processOnLocalhost :: JobInfo -> OnFailAction -> String -> ResultsChan -> IO ()
processOnLocalhost job onFail command resultChan = do
    let opts = (shell command) {std_out = CreatePipe, std_err = CreatePipe}
    withCreateProcess opts $ \_ (Just stdout) (Just stderr) process -> do
      forkIO $ retrieveOutput StderrLine stderr
      retrieveOutput StdoutLine stdout
      ec <- waitForProcess process
      writeChan resultChan (job, Exited ec onFail)
  where
    retrieveOutput cons handle = do
      sourceHandle handle =$= C.decodeUtf8 =$= C.linesUnbounded $$ CL.mapM_ $ \line ->
        writeChan resultChan (job, cons line)
      hClose handle

executeJob :: HostsPool -> Queue -> JobType -> JobInfo -> ResultsChan -> Daemon ()
executeJob counters q jt job resultChan = Monitoring.timed "batchd.job.duration" $ do
  cfg <- askConfig
  let mbHostName = getHostName q jt job
      jid = JobKey (Sql.SqlBackendKey $ jiId job)
  liftIO $ writeChan resultChan (job, StartExecution)
  case mbHostName of
    Nothing -> do -- localhost
      let command = getCommand Nothing jt job
      liftIO $ processOnLocalhost job (jtOnFail jt) command resultChan

    Just hostname -> do
      hostR <- liftIO $ loadHost hostname
      case hostR of
        Right host -> do
          let command = getCommand (Just host) jt job
          now <- liftIO $ getCurrentTime
          -- let result = JobResult jid now (ExitFailure (-2)) T.empty T.empty
          processOnHost counters host jt job resultChan command
          return ()
        Left err -> do
          $reportError "Error while executing job: {}" (Single $ Shown err)
          liftIO $ writeChan resultChan (job, ExecError (T.pack $ show err) (jtOnFail jt))
          return ()


