{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Batchd.Daemon.SSH where

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Format.Heavy
import Network.SSH.Client.LibSSH2
import System.FilePath
import System.Environment
import System.Exit

import Batchd.Core.Common.Types
import Batchd.Core.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Core.Daemon.Logging
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

processOnHost :: HostCounters -> Host -> JobType -> JobInfo -> String -> Daemon (ExitCode, T.Text)
processOnHost counters h jtype job command = do
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
    $debug "Target host settings: {}" (Single $ Shown h)
    r <- withHost counters h jtype $ do
            lts <- askLoggingStateM
            controller <- liftIO $ loadHostController lts (hController h)
            mbActualHostName <- getActualHostName_ controller (hControllerId h)
            hostname <- case mbActualHostName of
                          Nothing -> return original_hostname
                          Just actual -> do
                            $debug "Actual hostname of host `{}' is {}" (hName h, actual)
                            return actual

            wrapDaemon (withSSH2 known_hosts public_key private_key passphrase user hostname port) $ \session -> do
                $info "Connected to {}:{}." (hostname, port)
                liftIO $ execCommands session (hStartupCommands h)
                           `catch` (\(e :: SomeException) -> throw (ExecException e))
                uploadFiles (getInputFiles jtype job) (hInputDirectory h) session
                $info "EXECUTING: {}" (Single command)
                (ec,out) <- liftIO $ execCommands session [command]
                $info "Done." ()
                downloadFiles (hOutputDirectory h) (getOutputFiles jtype job) session
                let outText = TL.toStrict $ TLE.decodeUtf8 (head out)
                    ec' = if ec == 0
                            then ExitSuccess
                            else ExitFailure ec
                return (ec', outText)
    case r of
      Left e -> do
        $reportError "Error while executing job at host `{}': {}" (hName h, show e)
        return (ExitFailure (-1), T.pack (show e))
      Right result -> return result
  where
    getActualHostName_ (AnyHostController controller) name = do
      liftIO $ getActualHostName controller name

getInputFiles :: JobType -> JobInfo -> [FilePath]
getInputFiles jt job =
  [value | (name, value) <- M.assocs (jiParams job), getParamType jt name == Just InputFile]

getOutputFiles :: JobType -> JobInfo -> [FilePath]
getOutputFiles jt job =
  [value | (name, value) <- M.assocs (jiParams job), getParamType jt name == Just OutputFile]

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

