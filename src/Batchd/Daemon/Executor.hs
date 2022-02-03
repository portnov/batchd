{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Executor where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.Conduit
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
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
import System.Exit (ExitCode (..))
import Network.SSH.Client.LibSSH2.Conduit (execCommand)

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Daemon.SSH
import Batchd.Daemon.Hosts (loadHostController, withHost)
import Batchd.Daemon.Monitoring as Monitoring

getCommand :: GlobalConfig -> Maybe Host -> JobType -> JobInfo -> String
getCommand cfg mbHost jt job =
    TL.unpack $ format (parseShellFormat' $ TL.pack $ jtTemplate jt) (mkContext $ hostContext mbHost jt $ jiParams job)
  where
    mkContext m = optional $ m `ThenCheck` hostVars `ThenCheck` dbcVariables cfg
    hostVars = maybe M.empty hVariables mbHost

getHostName :: Queue -> JobType -> JobInfo -> Maybe T.Text
getHostName q jt job = T.pack <$>
  msum [jiHostName job, jtHostName jt, queueHostName q]

hostContext :: Maybe Host -> JobType -> JobParamInfo -> JobParamInfo
hostContext Nothing jt params = params
hostContext (Just host) jt params = M.fromList $ map update $ M.assocs params
  where
    update (key, value) =
      case getParamType jt key of
        Just InputFile -> (key, T.pack $ hInputDirectory host </> takeFileName (T.unpack value))
        Just OutputFile -> (key, T.pack $ hOutputDirectory host </> takeFileName (T.unpack value))
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

processOnHost :: HostsPool -> Host -> JobType -> JobInfo -> ResultsChan -> String -> Daemon ExitCode
processOnHost counters host jtype job resultChan command = do
  lts <- askLoggingStateM
  controller <- liftIO $ loadHostController lts (hController host)
  r <- withHost counters host jtype $ do
        withSshOnHost controller host $ \session -> do
           uploadFiles (getInputFiles jtype job) (hInputDirectory host) session
           $info "EXECUTING: {}" (Single command)
           (Just commandHandle, commandOutput) <- liftIO $ execCommand True session command
           ec <- liftIO $ retrieveOutput job jtype commandHandle commandOutput resultChan
           $info "Done, exit code is {}." (Single $ show ec)
           downloadFiles (hOutputDirectory host) (getOutputFiles jtype job) session
           return ec
  case r of
    Left e -> do
      $reportError "Error while executing job at host `{}': {}" (hName host, show e)
      liftIO $ writeChan resultChan (job, ExecError (T.pack $ show e) (jtOnFail jtype))
      return $ ExitFailure (-1)
    Right result -> return result

executeJob :: HostsPool -> Queue -> JobType -> JobInfo -> ResultsChan -> Daemon ()
executeJob counters q jt job resultChan = do
  let mbHostName = getHostName q jt job
      hostForMetric = fromMaybe "localhost" mbHostName
  let metrics = ["batchd.job.duration", 
                 "batchd.job.duration.host." <> hostForMetric,
                 "batchd.job.duration.type." <> T.pack (jtName jt)]
  Monitoring.timedN metrics $ do
    cfg <- askConfig
    liftIO $ writeChan resultChan (job, StartExecution)
    let jid = JobKey (Sql.SqlBackendKey $ jiId job)
    case mbHostName of
      Nothing -> do -- localhost
        let command = getCommand cfg Nothing jt job
        liftIO $ processOnLocalhost job (jtOnFail jt) command resultChan

      Just hostname -> do
        hostR <- liftIO $ loadHost hostname
        case hostR of
          Right host -> do
            let command = getCommand cfg (Just host) jt job
            now <- liftIO $ getCurrentTime
            -- let result = JobResult jid now (ExitFailure (-2)) T.empty T.empty
            $(putMessage config_level) "Loaded host configuration: {}" (Single $ show host)
            processOnHost counters host jt job resultChan command
            return ()
          Left err -> do
            $reportError "Error while executing job: {}" (Single $ Shown err)
            liftIO $ writeChan resultChan (job, ExecError (T.pack $ show err) (jtOnFail jt))
            return ()

