{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Batchd.Client.Config where

import GHC.Generics
import Control.Exception
import Data.Yaml
import Data.Generics hiding (Generic)
import Data.Maybe
import Data.Text.Format.Heavy
import Text.Localize
import Text.Localize.IO () -- import instances only
import System.Environment
import System.Posix.User (getLoginName)

import Batchd.Core.Common.Config
import Batchd.Client.Types
import Batchd.Client.CmdLine

data ClientConfig = ClientConfig {
    ccManagerUrl :: Maybe String,
    ccQueue :: Maybe String,
    ccType :: Maybe String,
    ccHost :: Maybe String,
    ccDisableAuth :: Bool,
    ccCertificate :: Maybe FilePath,
    ccKey :: Maybe FilePath,
    ccCaCertificate :: Maybe FilePath,
    ccDisableServerCertificateCheck :: Bool,
    ccUsername :: Maybe String,
    ccPassword :: Maybe String
  }
  deriving (Show, Data, Typeable, Generic)

defaultConfig :: ClientConfig
defaultConfig = ClientConfig {
    ccManagerUrl = Nothing,
    ccQueue = Nothing,
    ccType = Nothing,
    ccHost = Nothing,
    ccDisableAuth = False,
    ccCertificate = Nothing,
    ccKey = Nothing,
    ccCaCertificate = Nothing,
    ccDisableServerCertificateCheck = False,
    ccUsername = Nothing,
    ccPassword = Nothing
  }

instance FromJSON ClientConfig where
  parseJSON (Object v) =
    ClientConfig
      <$> v .:? "manager_url"
      <*> v .:? "queue"
      <*> v .:? "type"
      <*> v .:? "host"
      <*> v .:? "disable_auth" .!= False
      <*> v .:? "certificate"
      <*> v .:? "key"
      <*> v .:? "ca_certificate"
      <*> v .:? "disable_server_certificate_check" .!= False
      <*> v .:? "username"
      <*> v .:? "password"

getConfigParam :: Maybe String -> String -> Maybe String -> String -> IO String
getConfigParam cmdline varname cfg dflt = do
  case cmdline of
    Just val -> return val
    Nothing -> do
      mbEnv <- lookupEnv varname
      case mbEnv of
        Just val -> return val
        Nothing -> return $ fromMaybe dflt cfg
    
getConfigParam' :: Maybe String -> String -> Maybe String -> IO (Maybe String)
getConfigParam' cmdline varname cfg = do
  case cmdline of
    Just val -> return $ Just val
    Nothing -> do
      mbEnv <- lookupEnv varname
      case mbEnv of
        Just val -> return $ Just val
        Nothing -> return cfg

loadClientConfig :: IO ClientConfig
loadClientConfig = do
  mbPath <- locateConfig "" "client.yaml"
  case mbPath of
    Nothing -> return defaultConfig
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> throw =<< ClientException `fmap` (__f "Can't parse client configuration file:\n{}" (Single $ show err))
        Right cfg -> return cfg
    
getManagerUrl :: CmdLine -> ClientConfig -> IO String
getManagerUrl (CmdLine o _) cfg =
  getConfigParam (managerUrl o) "BATCH_MANAGER_URL" (ccManagerUrl cfg) defaultUrl

getHostName :: CmdLine -> ClientConfig -> IO (Maybe String)
getHostName (CmdLine _ c) cfg =
  getConfigParam' (hostName c) "BATCH_HOST" (ccHost cfg)

getTypeName :: CmdLine -> ClientConfig -> IO String
getTypeName (CmdLine _ c) cfg =
  getConfigParam (typeName c) "BATCH_TYPE" (ccType cfg) defaultType

getQueueName :: CmdLine -> ClientConfig -> IO String
getQueueName (CmdLine _ c) cfg =
  getConfigParam (queueName c) "BATCH_QUEUE" (ccQueue cfg) defaultQueue

getUserName :: CmdLine -> ClientConfig -> IO String
getUserName (CmdLine o _) cfg = do
  osuser <- getLoginName
  getConfigParam (username o) "BATCH_USERNAME" (ccUsername cfg) osuser

