{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Local where

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
import Data.Time
import System.Process
import System.FilePath
import System.IO
import System.Exit (ExitCode (..))
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts

execLocalCommand :: LoggingTState -> T.Text -> IO ExitCode
execLocalCommand lts command = do
    infoIO lts $(here) "Executing on dispatcher host: {}" (Single command)
    let opts = (shell $ T.unpack command) {std_out = CreatePipe, std_err = CreatePipe}
    withCreateProcess opts $ \_ (Just stdout) (Just stderr) process -> do
      forkIO $ retrieveOutput "error" stderr
      retrieveOutput "output" stdout
      ec <- waitForProcess process
      infoIO lts $(here) "Exit code: {}" (Single $ show ec)
      return ec
  where
    retrieveOutput :: T.Text -> Handle -> IO ()
    retrieveOutput kind handle = do
      sourceHandle handle =$= C.decodeUtf8 =$= C.linesUnbounded $$ CL.mapM_ $ \line ->
        infoIO lts $(here) "{}> {}" (kind, line)
      hClose handle

execLocalCommands :: LoggingTState -> [T.Text] -> IO ExitCode
execLocalCommands _ [] = return ExitSuccess
execLocalCommands lts (command : commands) = do
  ec <- execLocalCommand lts command
  if ec == ExitSuccess
    then execLocalCommands lts commands
    else return ec

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

