{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Batchd.Ext.AWS
  (
    AWSEC2 (..),
    Selector (..)
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.AWS
import Control.Lens
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson as Aeson
import Network.AWS.Auth
import Network.AWS.EC2
import Network.AWS.Waiter
import System.Log.Heavy
import System.Log.Heavy.AWS

import Batchd.Core

data AWSEC2 = AWSEC2 {
    awsEnableStartStop :: Bool
  , awsCredentials :: Credentials
  , awsRegion :: Region
  , awsLogging :: LoggingTState
  }

instance FromJSON AWSEC2 where
  parseJSON (Object v) = do
    driver <- v .: "driver"
    when (driver /= ("awsec2" :: T.Text)) $
      fail $ "incorrect driver specification"
    enable <- v .:? "enable_start_stop" .!= True
    creds <- v .:? "credentials" .!= Discover
    region <- read <$> v .: "region"
    return $ AWSEC2 enable creds region undefined

instance FromJSON Credentials where
  parseJSON (Aeson.String "discover") = return Discover
  parseJSON (Object v) = pFromFile v <|> pFromKeys v
    where
      pFromFile v = do
        file <- v .: "path"
        profile <- v .: "profile"
        return $ FromFile profile file

      pFromKeys v = do
        access <- TE.encodeUtf8 <$> (v .: "access_key")
        secret <- TE.encodeUtf8 <$> (v .: "secret_key")
        return $ FromKeys (AccessKey access) (SecretKey secret)
        

describe instanceId =
  describeInstances & (diiInstanceIds .~ [instanceId])

instance Show AWSEC2 where
  show _ = "<AWS EC2 host controller>"

instance HostController AWSEC2 where
  data Selector AWSEC2 = AWSEC2Selector

  controllerName AWSEC2Selector = "awsec2"

  doesSupportStartStop aws = awsEnableStartStop aws

  tryInitController AWSEC2Selector lts name = do
    r <- loadHostControllerConfig name
    case r of
      Left err -> return $ Left err
      Right aws -> return $ Right $ aws {awsLogging = lts}

  startHost aws name = do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ ltsLogger $ awsLogging aws)
      let instanceId = T.pack name

      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
              send $ startInstances & (sInstanceIds .~ [instanceId])
              await instanceRunning $ describe instanceId
      when (rs /= AcceptSuccess) $ do
        fail $ "Cannot start instance: " ++ show rs

  stopHost aws name = do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ ltsLogger $ awsLogging aws)
      let instanceId = T.pack name
      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
                send $ stopInstances & (siInstanceIds .~ [instanceId])
                await instanceStopped $ describe instanceId
      when (rs /= AcceptSuccess) $ do
        fail $ "Cannot stop instance: " ++ show rs

