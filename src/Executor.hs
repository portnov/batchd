{-# LANGUAGE OverloadedStrings #-}

module Executor where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Template
import qualified Database.Persist.Sql as Sql
import Data.Time
import System.Process
import System.FilePath
import System.Exit

import CommonTypes
import Config
import Logging
import Database
import SSH

mkContext :: JobParamInfo -> Context
mkContext m key =
  T.pack $ fromMaybe "" $ M.lookup (T.unpack key) m

getCommand :: Maybe Host -> JobType -> JobInfo -> String
getCommand mbHost jt job =
  TL.unpack $ substitute (T.pack $ jtTemplate jt) (mkContext $ hostContext mbHost jt $ jiParams job)

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

executeJob :: GlobalConfig -> Queue -> JobType -> JobInfo -> IO JobResult
executeJob cfg q jt job = do
  let mbHostName = getHostName q jt job
      jid = JobKey (Sql.SqlBackendKey $ jiId job)
  case mbHostName of
    Nothing -> do -- localhost
      let command = getCommand Nothing jt job
      (ec, stdout, stderr) <- readCreateProcessWithExitCode (shell command) ""
      now <- getCurrentTime
      return $ JobResult jid now ec (T.pack stdout) (T.pack stderr)
    Just hostname -> do
      hostR <- loadHost hostname
      case hostR of
        Right host -> do
          let command = getCommand (Just host) jt job
          (ec, stdout) <- processOnHost host jt job command
          now <- getCurrentTime
          return $ JobResult jid now ec stdout T.empty
        Left err -> do
          reportErrorIO cfg $ show err
          now <- getCurrentTime
          return $ JobResult jid now (ExitFailure (-1)) T.empty (T.pack $ show err)


