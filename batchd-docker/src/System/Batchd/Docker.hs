{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
module System.Batchd.Docker
  (Docker (..),
   defaultDockerUrl
  ) where

import Control.Monad.Trans
import Control.Monad.Catch
import Control.Exception
import Data.Maybe
import Data.Typeable
import qualified Data.Text as T
import Data.Aeson
import System.Batchd
import Docker.Client

deriving instance Typeable DockerError
instance Exception DockerError

data Docker = Docker {
    dEnableStartStop :: Bool
  , dUnixSocket :: Maybe FilePath
  , dBaseUrl :: URL
  }
  deriving (Show)

instance FromJSON Docker where
  parseJSON (Object v) =
    Docker
    <$> v .:? "enable_start_stop" .!= True
    <*> v .:? "unix_socket"
    <*> v .:? "base_url" .!= defaultDockerUrl

defaultDockerUrl :: URL
defaultDockerUrl = baseUrl defaultClientOpts

getHttpHandler :: (MonadIO m, MonadMask m) => Docker -> m (HttpHandler m)
getHttpHandler d = do
  case dUnixSocket d of
    Nothing -> defaultHttpHandler
    Just path -> unixHttpHandler path

instance HostController Docker where
  doesSupportStartStop d = dEnableStartStop d

  startHost d name = do
    handler <- getHttpHandler d
    let opts = defaultClientOpts {baseUrl = dBaseUrl d}
    r <- runDockerT (opts, handler) $ do
           startContainer defaultStartOpts $ fromJust $ toContainerID (T.pack name)
    print r
    return ()

  stopHost d name = do
    handler <- getHttpHandler d
    let opts = defaultClientOpts {baseUrl = dBaseUrl d}
    r <- runDockerT (opts, handler) $ do
           stopContainer DefaultTimeout $ fromJust $ toContainerID (T.pack name)
    print r
    return ()
