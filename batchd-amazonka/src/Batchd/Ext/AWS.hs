{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module contains an implementation of host controller for batchd,
-- which controls AWS EC2 instances.
module Batchd.Ext.AWS
  (
    AwsEc2Settings (..),
    awsEc2Driver
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.Trans.AWS
import Control.Lens
import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Format.Heavy
import Data.Aeson as Aeson
import Network.AWS.Auth
import Network.AWS.EC2
import Network.AWS.Waiter
import System.Log.Heavy
import System.Log.Heavy.AWS

import Batchd.Core

-- | Settings of AWS EC2 host controller
data AwsEc2Settings = AwsEc2Settings {
    awsEnableStartStop :: Bool     -- ^ Automatic start\/stop can be disabled in config file
  , awsCredentials :: Credentials  -- ^ AWS credentials. We can parse either @"path" / "profile"@
                                   --   or @"access_key" / "secret_key"@ pair from config file
  , awsRegion :: Region
  }

instance FromJSON AwsEc2Settings where
  parseJSON (Object v) = do
    driver <- v .: "driver"
    when (driver /= ("awsec2" :: T.Text)) $
      fail $ "incorrect driver specification"
    enable <- v .:? "enable_start_stop" .!= True
    creds <- v .:? "credentials" .!= Discover
    region <- read <$> v .: "region"
    return $ AwsEc2Settings enable creds region

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

-- | AWS EC2 host controller
awsEc2Driver :: HostDriver
awsEc2Driver = controllerFromConfig "awsec2" mkAwsEc2

mkAwsEc2 :: AwsEc2Settings -> LoggingTState -> HostController
mkAwsEc2 aws lts = HostController {

  controllerDriverName = driverName awsEc2Driver,

  doesSupportStartStop = awsEnableStartStop aws,

  getActualHostName = \name -> do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ ltsLogger lts)
      let instanceId = T.pack name
      runResourceT . runAWST env . within (awsRegion aws) $ do
          r <- trying _Error $ send $ describe instanceId
          case r of
            Right dirs -> do
                let mbNames = map (view insPublicDNSName) $ concatMap (view rInstances) $ view dirsReservations dirs
                case mbNames of
                  (Just dnsName : _) -> return $ Just $ T.unpack dnsName
                  _ -> return Nothing
            Left err -> return Nothing,

  startHost = \host -> do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ ltsLogger lts)
      let instanceId = hControllerId host

      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
              r <- trying _Error $ send $ startInstances & (sInstanceIds .~ [instanceId])
              case r of
                Right _ -> do
                    accept <- await instanceRunning $ describe instanceId
                    return $ Right accept
                Left err -> do
                    return $ Left $ UnknownError (show err)
      case rs of
        Left err -> return $ Left err
        Right accept -> do
          if accept /= AcceptSuccess
            then return $ Left $ UnknownError $ "Cannot start instance: " ++ show accept
            else return $ Right (),

  stopHost = \name -> do
      env <- newEnv (awsCredentials aws) <&> set envLogger (toAwsLogger $ ltsLogger lts)
      let instanceId = T.pack name
      rs <- runResourceT . runAWST env . within (awsRegion aws) $ do
                r <- trying _Error $ send $ stopInstances & (siInstanceIds .~ [instanceId])
                case r of
                  Right _ -> do
                      accept <- await instanceStopped $ describe instanceId
                      return $ Right accept
                  Left err -> do
                      return $ Left $ UnknownError (show err)
      case rs of
        Left err -> return $ Left err
        Right accept -> do
          if accept /= AcceptSuccess
            then return $ Left $ UnknownError $ "Cannot stop instance: " ++ show accept
            else return $ Right ()
  }

