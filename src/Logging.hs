{-# LANGUAGE TemplateHaskell #-}

module Logging where

import Control.Monad.Reader
import Control.Monad.Logger
import qualified Data.Text as T

import CommonTypes
import Types

debug :: String -> ConnectionM ()
debug msg = do
  cfg <- asks ciDbConfig
  enableLogging cfg ( $(logDebug) $ T.pack msg ) 

info :: String -> ConnectionM ()
info msg = do
  cfg <- asks ciDbConfig
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

infoDB :: DbConfig -> String -> DB ()
infoDB cfg msg = do
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

reportError :: String -> ConnectionM ()
reportError msg = do
  cfg <- asks ciDbConfig
  enableLogging cfg ( $(logError) $ T.pack msg ) 

debugIO :: DbConfig -> String -> IO ()
debugIO cfg msg = do
  enableLogging cfg ( $(logDebug) $ T.pack msg ) 

infoIO :: DbConfig -> String -> IO ()
infoIO cfg msg = do
  enableLogging cfg ( $(logInfo) $ T.pack msg ) 

reportErrorIO :: DbConfig -> String -> IO ()
reportErrorIO cfg msg = do
  enableLogging cfg ( $(logError) $ T.pack msg ) 

