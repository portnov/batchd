{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Daemon.Executor where

import Control.Monad
import Control.Concurrent.MVar
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import Data.Text.Template
import Database.Persist
import qualified Database.Persist.Sql as Sql
import Data.Time
import System.Process
import System.FilePath
import System.Exit
import Pipes
import qualified Pipes.ByteString as PB

import Common.Types
import Common.Config
import Daemon.Types
import Daemon.Logging
import Common.Data
import Daemon.Database
import Daemon.Hosts
import Daemon.SSH (processOnHost)

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

executeJob :: GlobalConfig -> HostCounters -> Queue -> JobType -> JobInfo -> IO JobResult
executeJob cfg counters q jt job = do
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
          (mvar, stdout) <- processOnHost cfg counters host jt job command
          -- runEffect $ stdout >-> consumeOutput cfg jid hostname False
          runEffect $ stdout >-> PB.stdout
          ec <- takeMVar mvar
          now <- getCurrentTime
          return $ JobResult jid now ec T.empty T.empty
        Left err -> do
          $reportErrorDB cfg $ show err
          now <- getCurrentTime
          return $ JobResult jid now (ExitFailure (-1)) T.empty (T.pack $ show err)

consumeOutput :: GlobalConfig -> Key Job -> String -> Bool -> Consumer B.ByteString IO ()
consumeOutput gcfg jkey host isError = do
    pool <- liftIO $ getPool gcfg
    loop gcfg pool
    
  where
    
    loop gcfg pool = do
      chunk <- await
      liftIO $ runDBIO gcfg pool $ writeLog chunk
      loop gcfg pool

    writeLog :: B.ByteString -> DB ()
    writeLog chunk = do
      now <- liftIO $ getCurrentTime
      let record = JobLog jkey now  host isError chunk
      insert record
      return ()

