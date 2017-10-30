{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Executor where

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse.Shell
import qualified Database.Persist.Sql as Sql hiding (Single)
import Data.Time
import System.Process
import System.FilePath
import System.Exit

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Logging
import Batchd.Common.Data
import Batchd.Core.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Daemon.SSH

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

executeJob :: HostCounters -> Queue -> JobType -> JobInfo -> Daemon JobResult
executeJob counters q jt job = do
  cfg <- askConfig
  let mbHostName = getHostName q jt job
      jid = JobKey (Sql.SqlBackendKey $ jiId job)
  case mbHostName of
    Nothing -> do -- localhost
      let command = getCommand Nothing jt job
      (ec, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode (shell command) ""
      now <- liftIO $ getCurrentTime
      return $ JobResult jid now ec (T.pack stdout) (T.pack stderr)
    Just hostname -> do
      hostR <- liftIO $ loadHost hostname
      case hostR of
        Right host -> do
          let command = getCommand (Just host) jt job
          (ec, stdout) <- processOnHost counters host jt job command
          now <- liftIO $ getCurrentTime
          return $ JobResult jid now ec stdout T.empty
        Left err -> do
          $reportError "Error while executing job: {}" (Single $ Shown err)
          now <- liftIO $ getCurrentTime
          return $ JobResult jid now (ExitFailure (-1)) T.empty (T.pack $ show err)


