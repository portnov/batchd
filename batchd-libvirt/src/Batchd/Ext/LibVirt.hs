{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module contains an implementation of batchd host controller,
-- which controls VMs supported by LibVirt.
module Batchd.Ext.LibVirt
  (
    LibVirt (..),
    Selector (..)
  ) where

import Control.Monad (when)
import Control.Exception
import Control.Concurrent
import Data.Time
import qualified Data.Text as T
import Data.Text.Format.Heavy
import Data.Aeson
import System.Log.Heavy
import Batchd.Core
import System.LibVirt as V

-- | LibVirt host controller
data LibVirt = LibVirt {
    lvEnableStartStop :: Bool      -- ^ Automatic start\/stop of VMs can be disabled in config file.
  , lvConnectionString :: String   -- ^ Libvirt connection string. Default is @"qemu:///system"@.
  , lvLogging :: LoggingTState
  }

instance FromJSON LibVirt where
  parseJSON (Object v) = do
    driver <- v .: "driver"
    when (driver /= ("libvirt" :: T.Text)) $
      fail $ "incorrect driver specification"
    enable <- v .:? "enable_start_stop" .!= True
    conn <- v .:? "connection_string" .!= "qemu:///system"
    return $ LibVirt enable conn undefined

instance Show LibVirt where
   show _ = "<LibVirt host controller>"

instance HostController LibVirt where
  data Selector LibVirt = LibVirtSelector

  controllerName LibVirtSelector = "libvirt"

  doesSupportStartStop l = lvEnableStartStop l

  tryInitController LibVirtSelector lts name = do
    r <- loadHostControllerConfig name
    case r of
      Left err -> return $ Left err
      Right lib -> return $ Right $ lib {lvLogging = lts}

  startHost l name = do
    let lts = lvLogging l
    withConnection (lvConnectionString l) $ \conn -> do
        infoIO lts $(here) "Connection to libvirt URI {} succeeded" (Single $ lvConnectionString l)
        mbdom <- do
                 x <- try $ lookupDomainName conn name
                 case x of
                   Left (e :: V.Error) -> do
                                          reportErrorIO lts $(here) "Cannot get domain ID by name `{}': {}" (name, show e)
                                          return Nothing
                   Right dom -> return (Just dom)
        case mbdom of
          Just dom -> do
            di <- getDomainInfo dom
            debugIO lts $(here) "Domain information obtained: {}" (Single $ show di)
            case diState di of
              DomainRunning -> return $ Right ()
              DomainPaused -> resumeDomain dom >> (return $ Right ())
              DomainShutoff -> do
                       createDomain dom
                       return $ Right ()
              st -> return $ Left $ UnknownError $ "Don't know what to do with virtual domain " ++ name ++ " in state " ++ show st
          Nothing -> return $ Left $ UnknownError $ "Domain is not defined in hypervisor: " ++ name

  stopHost l name = do
    let lts = lvLogging l
    withConnection (lvConnectionString l) $ \conn -> do
        infoIO lts $(here) "Connection to libvirt URI {} succeeded" (Single $ lvConnectionString l)
        mbdom <- do
                 x <- try $ lookupDomainName conn name
                 case x of
                   Left (e :: V.Error) -> do
                                          reportErrorIO lts $(here) "Cannot get domain ID by name `{}': {}" (name, show e)
                                          return Nothing
                   Right dom -> return (Just dom)
        case mbdom of
          Nothing -> return $ Left $ UnknownError $ "No such domain on hypervisor: " ++ name
          Just dom -> do
            shutdownDomain dom
            return $ Right ()

