{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Daemon.SSH where

import Control.Monad
import Control.Exception
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.SSH.Client.LibSSH2
import System.FilePath
import System.Environment
import System.Exit

import Common.CommonTypes
import Daemon.Logging
import Daemon.Hosts

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

processOnHost :: GlobalConfig -> HostCounters -> Host -> JobType -> JobInfo -> String -> IO (ExitCode, T.Text)
processOnHost cfg counters h jtype job command = do
  known_hosts <- getKnownHosts
  def_public_key <- getDfltPublicKey
  def_private_key <- getDfltPrivateKey
  let passphrase = hPassphrase h
      public_key = fromMaybe def_public_key $ hPublicKey h
      private_key = fromMaybe def_private_key $ hPrivateKey h
      user = hUserName h
      port = hPort h
      hostname = hHostName h

  $infoDB cfg $ "CONNECTING TO " ++ hostname
  $debugDB cfg $ show h
  withHost counters h jtype $ do
    (withSSH2 known_hosts public_key private_key passphrase user hostname port $ \session -> do
        $infoDB cfg "Connected."
        execCommands session (hStartupCommands h)
          `catch` (\(e :: SomeException) -> throw (ExecException e))
        uploadFiles cfg (getInputFiles jtype job) (hInputDirectory h) session
        $infoDB cfg $ "EXECUTING: " ++ command
        (ec,out) <- execCommands session [command]
        $infoDB cfg "Done."
        downloadFiles cfg (hOutputDirectory h) (getOutputFiles jtype job) session
        let outText = TL.toStrict $ TLE.decodeUtf8 (head out)
            ec' = if ec == 0
                    then ExitSuccess
                    else ExitFailure ec
        return (ec', outText)
        )
          `catch`
            (\(e :: SomeException) -> do
                                      $reportErrorDB cfg $ show e
                                      return (ExitFailure (-1), T.pack (show e))
                                      )

getInputFiles :: JobType -> JobInfo -> [FilePath]
getInputFiles jt job =
  [value | (name, value) <- M.assocs (jiParams job), getParamType jt name == Just InputFile]

getOutputFiles :: JobType -> JobInfo -> [FilePath]
getOutputFiles jt job =
  [value | (name, value) <- M.assocs (jiParams job), getParamType jt name == Just OutputFile]

uploadFiles :: GlobalConfig -> [FilePath] -> FilePath -> Session -> IO ()
uploadFiles cfg files input_directory session =
  forM_ files $ \path -> do
    let remotePath = input_directory </> takeFileName path
    $infoDB cfg $ "Uploading: `" ++ path ++ "' to `" ++ remotePath ++ "'"
    size <- scpSendFile session 0o777 path remotePath
              `catch` (\(e :: SomeException) -> throw (UploadException path e))
    $debugDB cfg $ "Done (" ++ show size ++ " bytes)."

downloadFiles :: GlobalConfig -> FilePath -> [FilePath] -> Session -> IO ()
downloadFiles cfg output_directory files session =
  forM_ files $ \path -> do
    let remotePath = output_directory </> takeFileName path
    $infoDB cfg $ "Downloading: `" ++ remotePath ++ "' to `" ++ path ++ "'"
    scpReceiveFile session remotePath path
              `catch` (\(e :: SomeException) -> throw (DownloadException path e))
    $debugDB cfg "Done."

