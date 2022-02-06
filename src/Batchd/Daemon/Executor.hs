{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Executor where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse.Shell
import Data.Text.Format.Heavy.Parse.Braces (parseFormat')
import qualified Database.Persist.Sql as Sql hiding (Single)
-- import Data.Time
import System.FilePath
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
import Batchd.Daemon.Local (processOnLocalhost, withLocalScript)
import Batchd.Daemon.Hosts (loadHostController, withHost)
import Batchd.Daemon.Monitoring as Monitoring

getCommands :: GlobalConfig -> Maybe Host -> JobType -> JobInfo -> [T.Text]
getCommands cfg mbHost jt job =
    let context = mkContext $ hostContext mbHost jt $ jiParams job
        syntax = fromMaybe (dbcDefTemplateSyntax cfg) (jtSyntax jt)
        parse = case syntax of
                  Shell -> parseShellFormat'
                  Python -> parseFormat'
    in  [TL.toStrict $ format (parse $ TL.fromStrict line) context | line <- jtTemplate jt]
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

processOnHost :: HostsPool -> Host -> JobType -> JobInfo -> ResultsChan -> FilePath -> [T.Text] -> Daemon ExitCode
processOnHost counters host jtype job resultChan scriptsDir commands = do
  host' <- liftIO $ accountForDefaultHostKeys host
  lts <- askLoggingStateM
  controller <- liftIO $ loadHostController lts (hController host')
  r <- withHost counters host' jtype $ do
        withSshOnHost controller host' $ \session -> do
           withRemoteScript session scriptsDir (jiId job) commands $ \command -> do
             uploadFiles (getInputFiles jtype job) (hInputDirectory host') session
             $info "EXECUTING: {}" (Single command)
             (Just commandHandle, commandOutput) <- liftIO $ execCommand True session command
             ec <- liftIO $ retrieveOutput job jtype commandHandle commandOutput resultChan
             $info "Done, exit code is {}." (Single $ show ec)
             downloadFiles (hOutputDirectory host') (getOutputFiles jtype job) session
             return ec
  case r of
    Left e -> do
      $reportError "Error while executing job at host `{}': {}" (hName host', show e)
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
        let commands = getCommands cfg Nothing jt job
        let scriptsDir = dbcDefScriptsDirectory cfg
        liftIO $ withLocalScript scriptsDir (jiId job) commands $ \script ->
                  processOnLocalhost job (jtOnFail jt) script resultChan

      Just hostname -> do
        hostR <- liftIO $ loadHost hostname
        case hostR of
          Right host -> do
            let commands = getCommands cfg (Just host) jt job
            let scriptsDir = fromMaybe (dbcDefScriptsDirectory cfg) (hScriptsDirectory host)
            -- now <- liftIO $ getCurrentTime
            -- let result = JobResult jid now (ExitFailure (-2)) T.empty T.empty
            $(putMessage config_level) "Loaded host configuration: {}" (Single $ show host)
            processOnHost counters host jt job resultChan scriptsDir commands
            return ()
          Left err -> do
            $reportError "Error while executing job: {}" (Single $ Shown err)
            liftIO $ writeChan resultChan (job, ExecError (T.pack $ show err) (jtOnFail jt))
            return ()

