{-# LANGUAGE TemplateHaskell #-}

module Logging where

import Control.Monad.Reader
import Control.Monad.Logger
import qualified Data.Text as T

import CommonTypes
import Types

debug :: String -> ConnectionM ()
debug msg = do
  cfg <- asks ciGlobalConfig
  enableLogging cfg ( $(logDebug) $ T.pack msg ) 

info :: String -> ConnectionM ()
info msg = do
  cfg <- asks ciGlobalConfig
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

infoDB :: GlobalConfig -> String -> DB ()
infoDB cfg msg = do
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

reportError :: String -> ConnectionM ()
reportError msg = do
  cfg <- asks ciGlobalConfig
  enableLogging cfg ( $(logError) $ T.pack msg ) 

debugIO :: GlobalConfig -> String -> IO ()
debugIO cfg msg = do
  enableLogging cfg ( $(logDebug) $ T.pack msg ) 

debugDB :: GlobalConfig -> String -> DB ()
debugDB cfg msg = do
  enableLogging cfg ( $(logDebug) $ T.pack msg ) 

infoIO :: GlobalConfig -> String -> IO ()
infoIO cfg msg = do
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

reportErrorIO :: GlobalConfig -> String -> IO ()
reportErrorIO cfg msg = do
  enableLogging cfg ( $(logError) $ T.pack msg ) 

