{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Batchd.AWS
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
import System.Batchd

data AWSEC2 = AWSEC2 {
    awsEnableStartStop :: Bool
  , awsCredentials :: Credentials
  , awsRegion :: Region
  , awsLogger :: SpecializedLogger
  }

instance FromJSON AWSEC2 where
  parseJSON (Object v) =
    AWSEC2
    <$> v .:? "enable_start_stop" .!= True
    <*> v .:? "credentials" .!= Discover
    <*> (read <$> v .: "region")
    <*> pure (const $ return ())

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

instance HostController AWSEC2 where
  data Selector AWSEC2 = AWSEC2Selector FilePath

  doesSupportStartStop aws = awsEnableStartStop aws

  initController (AWSEC2Selector name) logger = do
    r <- loadHostControllerConfig name
    case r of
      Left err -> throw err
      Right aws -> return $ aws {awsLogger = logger}

  startHost aws name = do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ awsLogger aws)
      let instanceId = T.pack name

      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
              send $ startInstances & (sInstanceIds .~ [instanceId])
              await instanceRunning $ describe instanceId
      when (rs /= AcceptSuccess) $ do
        fail $ "Cannot start instance: " ++ show rs

  stopHost aws name = do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ awsLogger aws)
      let instanceId = T.pack name
      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
                send $ stopInstances & (siInstanceIds .~ [instanceId])
                await instanceStopped $ describe instanceId
      when (rs /= AcceptSuccess) $ do
        fail $ "Cannot stop instance: " ++ show rs
