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
import System.Log.Heavy
import System.FilePath
import System.Environment
import System.Exit

import Batchd.Core.Common.Types
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types

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

withSshOnHost :: HostController -> Host -> (Session -> Daemon a) -> Daemon a
withSshOnHost controller host actions = do
    known_hosts <- liftIO getKnownHosts
    def_public_key <- liftIO getDfltPublicKey
    def_private_key <- liftIO getDfltPrivateKey
    let passphrase = hPassphrase host
        public_key = fromMaybe def_public_key $ hPublicKey host
        private_key = fromMaybe def_private_key $ hPrivateKey host
        user = hUserName host
        port = hPort host
        original_hostname = hHostName host

    $(putMessage config_level) "Target host settings: {}" (Single $ Shown host)
    mbActualHostName <- liftIO $ getActualHostName controller (hControllerId host)
    hostname <- case mbActualHostName of
                  Nothing -> return original_hostname
                  Just actual -> do
                    $debug "Actual hostname of host `{}' is {}" (hName host, actual)
                    return actual
    $info "CONNECTING TO {} => {}@{}:{}" (original_hostname, user, hostname, port)
    wrapDaemon (withSSH2 known_hosts public_key private_key (T.unpack passphrase) (T.unpack user) (T.unpack hostname) port) $ \session -> do
      $debug "Connected to {}:{}." (hostname, port)
      actions session

execCommandsOnHost :: HostController -> Host -> [T.Text] -> Daemon ExitCode
execCommandsOnHost controller host commands =
      withSshOnHost controller host $ \session -> go ExitSuccess session commands
  where
    go :: ExitCode -> Session -> [T.Text] -> Daemon ExitCode
    go prevEc _ [] = return prevEc
    go prevEc session (command : commands) = do
      $info "Executing: {}" (Single command)
      (Just commandHandle, commandOutput) <- liftIO $ execCommand True session (T.unpack command)
      lts <- askLoggingStateM
      liftIO $
          commandOutput =$= C.decodeUtf8 =$= C.linesUnbounded $$ CL.mapM_ $ \line ->
            infoIO lts $(here) "output> {}" (Single line)
      rc <- liftIO $ getReturnCode commandHandle
      $info "Exit code: {}" (Single rc)
      if rc == 0
        then go ExitSuccess session commands
        else return (ExitFailure rc)

retrieveOutput :: JobInfo -> JobType -> CommandsHandle -> Source IO B.ByteString -> ResultsChan -> IO ExitCode
retrieveOutput job jtype commandHandle commandOutput resultChan = do
    commandOutput =$= C.decodeUtf8 =$= C.linesUnbounded $$ CL.mapM_ $ \line ->
        writeChan resultChan (job, StdoutLine line)
    rc <- getReturnCode commandHandle
    let ec = if rc == 0
               then ExitSuccess
               else ExitFailure rc
    writeChan resultChan (job, Exited ec (jtOnFail jtype))
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

