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

import CommonTypes
import Types
import Database
import SSH

mkContext :: JobParamInfo -> Context
mkContext m key =
  T.pack $ fromMaybe "" $ M.lookup (T.unpack key) m

getCommand :: JobType -> JobInfo -> String
getCommand jt job = TL.unpack $ substitute (T.pack $ jtTemplate jt) (mkContext $ jiParams job)

getHostName :: Queue -> JobType -> JobInfo -> Maybe String
getHostName q jt job =
  msum [jiHostName job, jtHostName jt, queueHostName q]

executeJob :: Queue -> JobType -> JobInfo -> IO JobResult
executeJob q jt job = do
  let command = getCommand jt job
      mbHost = getHostName q jt job
      jid = JobKey (Sql.SqlBackendKey $ jiId job)
  case mbHost of
    Nothing -> do -- localhost
      (ec, stdout, stderr) <- readCreateProcessWithExitCode (shell command) ""
      now <- getCurrentTime
      return $ JobResult jid now ec (T.pack stdout) (T.pack stderr)
    Just hostname -> do
      host <- loadHost hostname
      (ec, stdout) <- processOnHost host jt job command
      now <- getCurrentTime
      return $ JobResult jid now ec stdout T.empty


