{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module contains an implementation of host controller for batchd,
-- which controls Linode.com instances.
module Batchd.Ext.Linode
  (
    LinodeSettings (..),
    linodeDriver
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans
import Control.Lens
import Control.Exception
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M
import Data.Text.Format.Heavy
import Data.Aeson as Aeson
import System.Log.Heavy
import Network.HTTP.Client
import qualified Linode as L

import Batchd.Core

deriving instance MonadFail m => MonadFail (L.ClientT m)

data LinodeSettings = LinodeSettings {
    linodeEnableStartStop :: Bool
  , linodeTokenFile :: FilePath
  }

instance FromJSON LinodeSettings where
  parseJSON (Object v) = do
    driver <- v .: "driver"
    when (driver /= ("linode" :: T.Text)) $
      fail $ "incorrect driver specification"
    enable <- v .:? "enable_start_stop" .!= True
    tokenFile <- v .: "token_file"
    return $ LinodeSettings enable tokenFile

linodeDriver :: HostDriver
linodeDriver = controllerFromConfig "linode" mkLinode

mkLinode :: LinodeSettings -> LoggingTState -> HostController
mkLinode settings lts = HostController {
    controllerDriverName = driverName linodeDriver,

    doesSupportStartStop = linodeEnableStartStop settings,

    getActualHostName = \labelStr -> withConfiguration settings $ lookupLinodeIp (T.pack labelStr),

    startHost = \labelStr -> withConfiguration settings $ do
                                linodeId <- lookupLinodeId (T.pack labelStr)
                                status <- getInstanceStatus linodeId
                                case status of
                                  L.LinodeStatus'EnumOffline -> bootInstanceById linodeId
                                  L.LinodeStatus'EnumRunning -> do
                                    infoIO lts $(here) "Instance {} is already running" (Single linodeId)
                                    return $ Right ()
                                  _ -> return $ Left $ UnknownError $ "Don't know what do do with instance in state " ++ show status ,

    stopHost = \labelStr -> withConfiguration settings $ shutdownInstance (T.pack labelStr)
  }

mkConfiguration :: LinodeSettings -> IO L.Configuration
mkConfiguration settings = do
 text <- TIO.readFile (linodeTokenFile settings)
 let token = head $ T.words text
 return $ L.defaultConfiguration {L.configSecurityScheme = L.bearerAuthenticationSecurityScheme token}

withConfiguration :: LinodeSettings -> L.ClientT IO a -> IO a
withConfiguration settings action = do
  cfg <- mkConfiguration settings
  L.runWithConfiguration cfg action

listLinodes :: L.ClientT IO [L.Linode]
listLinodes = do
  let rq = L.mkGetLinodeInstancesParameters
  rs <- L.getLinodeInstances rq
  case responseBody rs of
    L.GetLinodeInstancesResponse200 body ->
      return $ fromMaybe [] $ L.getLinodeInstancesResponseBody200Data body
    L.GetLinodeInstancesResponseError str -> fail str
    L.GetLinodeInstancesResponseDefault err -> fail $ show err

lookupLinodeId :: T.Text -> L.ClientT IO Int
lookupLinodeId label = do
  linodes <- listLinodes
  let pairs = [(L.linodeLabel l, L.linodeId l) | l <- linodes]
      m = M.fromList [(label, id) | (Just label, Just id) <- pairs]
  case M.lookup label m of
    Nothing -> fail "Cannot find Linode Instance by specified label"
    Just l -> return l

lookupLinodeIp :: T.Text -> L.ClientT IO (Maybe String)
lookupLinodeIp label = do
  linodes <- listLinodes
  let pairs = [(L.linodeLabel l, L.linodeIpv4 l) | l <- linodes]
      m = M.fromList [(label, head ips) | (Just label, Just ips) <- pairs]
  case M.lookup label m of
    Nothing -> fail "Cannot find Linode Instance by specified label"
    Just ip -> return (Just $ T.unpack ip)

getInstanceStatus :: Int -> L.ClientT IO L.LinodeStatus'
getInstanceStatus linodeId = do
  rs <- L.getLinodeInstance linodeId
  case responseBody rs of
    L.GetLinodeInstanceResponse200 linode ->
      case L.linodeStatus linode of
        Just status -> return status
        Nothing -> fail "Instance status is not specified in service response"
    L.GetLinodeInstanceResponseError str -> fail str
    L.GetLinodeInstanceResponseDefault err -> fail $ show err

bootInstance :: T.Text -> L.ClientT IO (Either Error ())
bootInstance label = do
  linodeId <- lookupLinodeId label
  bootInstanceById linodeId

bootInstanceById :: Int -> L.ClientT IO (Either Error ())
bootInstanceById linodeId = do
  rs <- L.bootLinodeInstance linodeId Nothing
  case responseBody rs of
    L.BootLinodeInstanceResponse200 obj -> return $ Right ()
    L.BootLinodeInstanceResponseError str -> return $ Left $ UnknownError str
    L.BootLinodeInstanceResponseDefault err -> return $ Left $ UnknownError $ show err

shutdownInstance :: T.Text -> L.ClientT IO (Either Error ())
shutdownInstance label = do
  linodeId <- lookupLinodeId label
  rs <- L.shutdownLinodeInstance linodeId
  case responseBody rs of
    L.ShutdownLinodeInstanceResponse200 obj -> return $ Right ()
    L.ShutdownLinodeInstanceResponseError str -> return $ Left $ UnknownError str
    L.ShutdownLinodeInstanceResponseDefault err -> return $ Left $ UnknownError $ show err

