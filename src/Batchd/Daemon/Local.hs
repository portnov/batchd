{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Local where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import qualified Control.Exception as E
import Data.Conduit
import Data.Maybe (fromMaybe)
import Data.Int
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.Conduit.Binary (sourceHandle)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse.Shell
import Data.Time
import System.Process
import System.FilePath
import System.Directory
import System.IO
import System.Exit (ExitCode (..))
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Core.Daemon.Logging
import Batchd.Core.Daemon.Hosts
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Daemon.Types

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

withLocalScript :: FilePath -> Int64 -> [T.Text] -> (String -> IO ()) -> IO ()
withLocalScript scriptsDir jobId commands actions = do
  if length commands == 1
    then actions (T.unpack $ head commands)
    else do
      let scriptText = formatScript commands
          scriptName = TL.unpack $ format "batchd_job_{}.script" (Single jobId)
          scriptPath = scriptsDir </> scriptName
          chmodX = do
            perm <- getPermissions scriptPath
            setPermissions scriptPath (setOwnerExecutable True perm)
      E.bracket_ (TIO.writeFile scriptPath scriptText >> chmodX)
                 (removeFile scriptPath)
                 (actions scriptPath)

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

