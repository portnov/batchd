{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Batchd.Daemon.SSH where

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import Data.Text.Format.Heavy
import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Conduit
import System.FilePath
import System.Environment
import System.Exit

import Batchd.Core.Common.Types
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Daemon.Hosts

getKnownHosts :: IO FilePath
getKnownHosts = do
  home <- getEnv "HOME"
  return $ home </> ".ssh" </> "known_hosts"

getDfltPublicKey :: IO FilePath
getDfltPublicKey = do
  home <- getEnv "HOME"
  return $ home </> ".ssh" </> "id_rsa.pub"

getDfltPrivateKey :: IO FilePath
getDfltPrivateKey = do
  home <- getEnv "HOME"
  return $ home </> ".ssh" </> "id_rsa"

processOnHost :: HostsPool -> Host -> JobType -> JobInfo -> ResultsChan -> String -> Daemon ExitCode
processOnHost counters h jtype job resultChan command = do
    cfg <- askConfig
    known_hosts <- liftIO $ getKnownHosts
    def_public_key <- liftIO $ getDfltPublicKey
    def_private_key <- liftIO $ getDfltPrivateKey
    let passphrase = hPassphrase h
        public_key = fromMaybe def_public_key $ hPublicKey h
        private_key = fromMaybe def_private_key $ hPrivateKey h
        user = hUserName h
        port = hPort h
        original_hostname = hHostName h

    $info "CONNECTING TO {}:{}" (original_hostname, port)
    $(putMessage config_level) "Target host settings: {}" (Single $ Shown h)
    r <- withHost counters h jtype $ do
            lts <- askLoggingStateM
            controller <- liftIO $ loadHostController lts (hController h)
            mbActualHostName <- liftIO $ getActualHostName controller (hControllerId h)
            hostname <- case mbActualHostName of
                          Nothing -> return original_hostname
                          Just actual -> do
                            $debug "Actual hostname of host `{}' is {}" (hName h, actual)
                            return actual

            wrapDaemon (withSSH2 known_hosts public_key private_key passphrase user hostname port) $ \session -> do
                $info "Connected to {}:{}." (hostname, port)
--                 liftIO $ execCommands session (hStartupCommands h)
--                            `catch` (\(e :: SomeException) -> throw (ExecException e))
                uploadFiles (getInputFiles jtype job) (hInputDirectory h) session
                $info "EXECUTING: {}" (Single command)
                (Just commandHandle, commandOutput) <- liftIO $ execCommand True session command
                ec <- retrieveOutput job jtype commandHandle commandOutput resultChan
                $info "Done, exit code is {}." (Single $ show ec)
                downloadFiles (hOutputDirectory h) (getOutputFiles jtype job) session
                return ec
    case r of
      Left e -> do
        $reportError "Error while executing job at host `{}': {}" (hName h, show e)
        liftIO $ writeChan resultChan (job, ExecError (T.pack $ show e) (jtOnFail jtype))
        return $ ExitFailure (-1)
      Right result -> return result

retrieveOutput :: JobInfo -> JobType -> CommandsHandle -> Source IO B.ByteString -> ResultsChan -> Daemon ExitCode
retrieveOutput job jtype commandHandle commandOutput resultChan = do
    liftIO $ do
        commandOutput =$= C.decodeUtf8 =$= C.linesUnbounded $$ CL.mapM_ $ \line ->
            writeChan resultChan (job, StdoutLine line)
    rc <- liftIO $ getReturnCode commandHandle
    let ec = if rc == 0
               then ExitSuccess
               else ExitFailure rc
    liftIO $ writeChan resultChan (job, Exited ec (jtOnFail jtype))
    return ec

getInputFiles :: JobType -> JobInfo -> [FilePath]
getInputFiles jt job =
  [T.unpack value | (name, value) <- M.assocs (jiParams job), getParamType jt name == Just InputFile]

getOutputFiles :: JobType -> JobInfo -> [FilePath]
getOutputFiles jt job =
  [T.unpack value | (name, value) <- M.assocs (jiParams job), getParamType jt name == Just OutputFile]

uploadFiles :: [FilePath] -> FilePath -> Session -> Daemon ()
uploadFiles files input_directory session =
  forM_ files $ \path -> do
    let remotePath = input_directory </> takeFileName path
    $info "Uploading `{}' to `{}'" (path, remotePath)
    size <- liftIO $ scpSendFile session 0o777 path remotePath
                       `catch` (\(e :: SomeException) -> throw (UploadException path e))
    $debug "Done ({} bytes)." (Single size)

downloadFiles :: FilePath -> [FilePath] -> Session -> Daemon ()
downloadFiles output_directory files session =
  forM_ files $ \path -> do
    let remotePath = output_directory </> takeFileName path
    $info "Downlooading `{}' to `{}'" (remotePath, path)
    liftIO $ scpReceiveFile session remotePath path
              `catch` (\(e :: SomeException) -> throw (DownloadException path e))
    $debug "Done." ()

