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
import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Exception
import Data.Maybe
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Map as M
import Data.Text.Format.Heavy
import Data.Aeson as Aeson
import Data.Aeson.Types
import System.Log.Heavy
import Network.HTTP.Client
import qualified Linode as L

import Batchd.Core

deriving instance MonadFail m => MonadFail (L.ClientT m)

data ControlActions =
    BootShutdown
  | CreateDelete
  deriving (Eq, Show)

data LinodeInstanceSettings = LinodeInstanceSettings {
      linodeInstanceType :: T.Text
    , linodeInstanceImage :: T.Text
    , linodeInstanceRegion :: T.Text
    , linodeRootPassword :: T.Text
  }
  deriving (Eq, Show)

data LinodeSettings = LinodeSettings {
    linodeEnableStartStop :: Bool
  , linodeTokenFile :: FilePath
  , linodeActions :: ControlActions
  }

instance FromJSON LinodeInstanceSettings where
  parseJSON (Object v) =
    LinodeInstanceSettings
      <$> v .: "type"
      <*> v .: "image"
      <*> v .: "region"
      <*> v .: "root_password"
  parseJSON invalid = typeMismatch "instance" invalid

instance FromJSON ControlActions where
  parseJSON (Aeson.String "boot_and_shutdown") = return BootShutdown
  parseJSON (Aeson.String "create_and_delete") = return CreateDelete
  parseJSON invalid = typeMismatch "actions" invalid

instance FromJSON LinodeSettings where
  parseJSON (Object v) = do
    driver <- v .: "driver"
    when (driver /= ("linode" :: T.Text)) $
      fail $ "incorrect driver specification"
    enable <- v .:? "enable_start_stop" .!= True
    actions <- v .:? "actions" .!= BootShutdown
    tokenFile <- v .: "token_file"
    return $ LinodeSettings enable tokenFile actions

linodeDriver :: HostDriver
linodeDriver = controllerFromConfig "linode" mkLinode

mkLinode :: LinodeSettings -> LoggingTState -> HostController
mkLinode settings lts = HostController {
    controllerDriverName = driverName linodeDriver,

    doesSupportStartStop = linodeEnableStartStop settings,

    getActualHostName = \label -> withConfiguration settings $ lookupLinodeIp label,

    startHost = \host -> withConfiguration settings $ do
                                case linodeActions settings of
                                  BootShutdown -> bootInstance lts (hControllerId host)
                                  CreateDelete -> createInstance lts host,

    stopHost = \label -> withConfiguration settings $ do
                                case linodeActions settings of
                                  BootShutdown -> shutdownInstance label
                                  CreateDelete -> deleteInstance lts label
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

lookupLinodeId' :: T.Text -> L.ClientT IO (Maybe Int)
lookupLinodeId' label = do
  linodes <- listLinodes
  let pairs = [(L.linodeLabel l, L.linodeId l) | l <- linodes]
      m = M.fromList [(label, id) | (Just label, Just id) <- pairs]
  return $ M.lookup label m

lookupLinodeId :: T.Text -> L.ClientT IO Int
lookupLinodeId label = do
  maybeId <- lookupLinodeId' label
  case maybeId of
    Nothing -> fail "Cannot find Linode Instance by specified label"
    Just l -> return l

lookupLinodeIp :: T.Text -> L.ClientT IO (Maybe T.Text)
lookupLinodeIp label = do
  linodes <- listLinodes
  let pairs = [(L.linodeLabel l, L.linodeIpv4 l) | l <- linodes]
      m = M.fromList [(label, head ips) | (Just label, Just ips) <- pairs]
  case M.lookup label m of
    Nothing -> fail "Cannot find Linode Instance by specified label"
    Just ip -> return (Just ip)

getInstanceStatus' :: T.Text -> L.ClientT IO (Maybe (Int, L.LinodeStatus'))
getInstanceStatus' label = do
  linodes <- listLinodes
  let pairs = [(L.linodeLabel l, L.linodeId l, L.linodeStatus l) | l <- linodes]
      m = M.fromList [(label, (id, status)) | (Just label, Just id, Just status) <- pairs]
  return $ M.lookup label m

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

waitForStatus :: LoggingTState -> Int -> L.LinodeStatus' -> L.ClientT IO (Either Error ())
waitForStatus lts linodeId target = do
    startTime <- liftIO getCurrentTime
    wait startTime
  where
    wait startTime = do
      st <- getInstanceStatus linodeId
      if st == target
        then return $ Right ()
        else do
          now <- liftIO getCurrentTime
          if ceiling (diffUTCTime now startTime) >= 60
            then return $ Left $ UnknownError $ "Startup timeout"
            else do
                 liftIO $ debugIO lts $(here) "Linode #{} status is {}, wait..." (linodeId, show st)
                 liftIO $ threadDelay $ 10 * 1000 * 1000
                 wait startTime

bootInstance :: LoggingTState -> T.Text -> L.ClientT IO (Either Error ())
bootInstance lts label = do
  linodeId <- lookupLinodeId label
  status <- getInstanceStatus linodeId
  case status of
    L.LinodeStatus'EnumOffline -> bootInstanceById linodeId
    L.LinodeStatus'EnumRunning -> do
      infoIO lts $(here) "Instance {} is already running" (Single linodeId)
      return $ Right ()
    _ -> return $ Left $ UnknownError $ "Don't know what do do with instance in state " ++ show status

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

withSettings :: Host -> (LinodeInstanceSettings -> L.ClientT IO (Either Error a)) -> L.ClientT IO (Either Error a)
withSettings host actions =
  case hControllerSpecific host of
    Nothing -> return $ Left $ UnknownError "Linode specific settings are not provided"
    Just value -> case fromJSON value of
                    Error str -> return $ Left $ UnknownError $ "Can't parse Linode-specific settings: " ++ str
                    Success settings -> actions settings

createInstance :: LoggingTState -> Host -> L.ClientT IO (Either Error ())
createInstance lts host = do
  let label = hControllerId host
  mbStatus <- getInstanceStatus' label
  case mbStatus of
    Nothing -> do
      publicKeys <- case hPublicKey host of
                      Nothing -> return []
                      Just path -> do
                        text <- liftIO $ TIO.readFile path
                        return [T.strip text]
      withSettings host $ \settings -> do
          let rq = L.mkCreateLinodeInstanceRequestBody {
                      L.createLinodeInstanceRequestBodyType = Just (linodeInstanceType settings)
                    , L.createLinodeInstanceRequestBodyImage = Just (linodeInstanceImage settings)
                    , L.createLinodeInstanceRequestBodyRegion = Just (linodeInstanceRegion settings)
                    , L.createLinodeInstanceRequestBodyLabel = Just label
                    , L.createLinodeInstanceRequestBodyAuthorizedKeys = Just publicKeys
                    --, L.createLinodeInstanceRequestBodyAuthorizedUsers = Just [hUserName host]
                    , L.createLinodeInstanceRequestBodyRootPass = Just (linodeRootPassword settings)
                  }
          rs <- L.createLinodeInstance rq
          case responseBody rs of
            L.CreateLinodeInstanceResponse200 linode -> do
              let ips = maybe "<no>" (T.intercalate ", ") $ L.linodeIpv4 linode
              infoIO lts $(here) "Created new Linode instance `{}', ID = {}, IP {}" (L.linodeLabel linode, L.linodeId linode, ips)
              waitForStatus lts (fromJust $ L.linodeId linode) L.LinodeStatus'EnumRunning
            L.CreateLinodeInstanceResponseError str -> return $ Left $ UnknownError str
            L.CreateLinodeInstanceResponseDefault err -> return $ Left $ UnknownError $ show err

    Just (linodeId, L.LinodeStatus'EnumOffline) -> bootInstanceById linodeId
    Just (_, L.LinodeStatus'EnumRunning) -> return $ Right ()
    Just (_, status) -> return $ Left $ UnknownError $ "Don't know what do do with instance in state " ++ show status

deleteInstance :: LoggingTState -> T.Text -> L.ClientT IO (Either Error ())
deleteInstance lts label = do
  linodeId <- lookupLinodeId label
  rs <- L.deleteLinodeInstance linodeId
  case responseBody rs of
    L.DeleteLinodeInstanceResponse200 obj -> do
      infoIO lts $(here) "Deleted Linode instance {}" (Single label)
      return $ Right ()
    L.DeleteLinodeInstanceResponseError str -> return $ Left $ UnknownError str
    L.DeleteLinodeInstanceResponseDefault err -> return $ Left $ UnknownError $ show err

