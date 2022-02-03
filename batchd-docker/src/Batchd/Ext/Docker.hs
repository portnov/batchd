{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This module contains an implementation of batchd host controllers,
-- which controls docker containers.
module Batchd.Ext.Docker
  (DockerSettings (..),
   dockerDriver,
   defaultDockerUrl
  ) where

import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Exception
import Data.Maybe
import Data.Typeable
import qualified Data.Text as T
import Data.Aeson
import Batchd.Core
import Docker.Client

deriving instance Typeable DockerError
instance Exception DockerError

-- | Settings of Docker host controller
data DockerSettings = DockerSettings {
    dEnableStartStop :: Bool       -- ^ Automatic start\/stop of containers can be disabled in config file
  , dUnixSocket :: Maybe FilePath  -- ^ This should be usually specified for default docker installation on local host
  , dBaseUrl :: URL                -- ^ Docker API URL
  }
  deriving (Show)

instance FromJSON DockerSettings where
  parseJSON (Object v) = do
    driver <- v .: "driver"
    when (driver /= ("docker" :: T.Text)) $
      fail $ "incorrect driver specification"
    enable <- v .:? "enable_start_stop" .!= True
    socket <- v .:? "unix_socket"
    url <- v .:? "base_url" .!= defaultDockerUrl
    return $ DockerSettings enable socket url

-- | Default Docker API URL.
defaultDockerUrl :: URL
defaultDockerUrl = baseUrl defaultClientOpts

getHttpHandler :: (MonadIO m, MonadMask m, MonadUnliftIO m) => DockerSettings -> m (HttpHandler m)
getHttpHandler d = do
  case dUnixSocket d of
    Nothing -> defaultHttpHandler
    Just path -> unixHttpHandler path

-- | Initialize Docker host controller
dockerDriver :: HostDriver
dockerDriver =
  controllerFromConfig "docker" $ \d lts -> HostController {

    controllerDriverName = driverName dockerDriver,

    getActualHostName = \_ -> return Nothing,

    doesSupportStartStop = dEnableStartStop d,

    startHost = \name -> do
      handler <- getHttpHandler d
      let opts = defaultClientOpts {baseUrl = dBaseUrl d}
      r <- runDockerT (opts, handler) $ do
             startContainer defaultStartOpts $ fromJust $ toContainerID name
      case r of
        Right _ -> return $ Right ()
        Left err -> return $ Left $ UnknownError $ show err,

    stopHost = \name -> do
      handler <- getHttpHandler d
      let opts = defaultClientOpts {baseUrl = dBaseUrl d}
      r <- runDockerT (opts, handler) $ do
             stopContainer DefaultTimeout $ fromJust $ toContainerID name
      case r of
        Right _ -> return $ Right ()
        Left err -> return $ Left $ UnknownError $ show err
  }

