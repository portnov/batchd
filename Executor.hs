{-# LANGUAGE OverloadedStrings #-}

module Executor where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Template
import qualified Database.Persist.Sql as Sql
import Data.Time
import System.Process

import Types
import Database

mkContext :: JobParamInfo -> Context
mkContext m key =
  T.pack $ fromMaybe "" $ M.lookup (T.unpack key) m

getCommand :: JobType -> JobInfo -> String
getCommand jt job = TL.unpack $ substitute (T.pack $ jtTemplate jt) (mkContext $ jiParams job)

executeJob :: JobType -> JobInfo -> IO JobResult
executeJob jt job = do
  let command = getCommand jt job
  (ec, stdout, stderr) <- readCreateProcessWithExitCode (shell command) ""
  let jid = JobKey (Sql.SqlBackendKey $ jiId job)
  now <- getCurrentTime
  return $ JobResult jid now ec (T.pack stdout) (T.pack stderr)
