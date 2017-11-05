{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
-- | This module contains defintion of @Client@ data type and utility functions for it.
module Batchd.Client.Monad where

import Control.Monad.State
import Control.Exception
import qualified Data.Text.Lazy as TL
import Network.HTTP.Client
import System.Log.Heavy
import qualified System.Log.FastLogger as FL

import Batchd.Core.Common.Localize
import Batchd.Common.Types
import Batchd.Client.Config
import Batchd.Client.Types
import Batchd.Client.CmdLine

-- | Client state
data ClientState = ClientState {
    csCmdline :: CmdLine                 -- ^ Parsed command line
  , csConfig :: ClientConfig             -- ^ Loaded configuration file
  , csCredentials :: Maybe Credentials   -- ^ Cached user credentials
  , csAuthMethods :: Maybe [AuthMethod]  -- ^ Cached list of authentication methods supported by server
  , csLogger :: Maybe SpecializedLogger  -- ^ Logging function
  , csManager :: Maybe Manager           -- ^ HTTP\/HTTPS manager
  }

-- | Client monad
type Client a = StateT ClientState IO a

instance Localized (StateT ClientState IO) where
  getLanguage = lift getLanguage
  getTranslations = lift getTranslations
  getContext = lift getContext

instance HasLogger (StateT ClientState IO) where
  getLogger = do
    mbLogger <- gets csLogger
    case mbLogger of
      Just logger -> return logger
      Nothing -> fail "Logger is not initialized yet"

  localLogger logger actions = do
    oldLogger <- gets csLogger
    modify $ \st -> st {csLogger = Just logger}
    result <- actions
    modify $ \st -> st {csLogger = oldLogger}
    return result

-- | Execute actions within Client monad.
runClient :: ClientState -> Client a -> IO a
runClient st action =
    evalStateT (withLogging (LoggingSettings logSettings) action) st
  where
    logSettings =
      filtering [([], verbosity)] $
        FastLoggerSettings logFormat (FL.LogStderr 0)
    verbosity = logLevel $ cmdCommon $ csCmdline st
    logFormat = "{level:~l}: {message}\n"

-- | Get manager base URL.
getBaseUrl :: Client String
getBaseUrl = do
  cfg <- gets csConfig
  opts <- gets csCmdline
  liftIO $ getManagerUrl opts cfg

-- | Throw an exception.
throwC :: TL.Text -> Client a
throwC msg = lift $ throw $ ClientException msg

wrapClient :: (forall a. IO a -> IO a) -> Client b -> Client b
wrapClient wrapper actions = do
  state <- get
  (result, state') <- liftIO $ wrapper $ do
                        runStateT actions state
  put state'
  return result

-- | Catch an exception.
catchC :: Exception e
            => Client a
            -> (e -> Client a)
            -> Client a
catchC action handler = do
    state <- get
    (result, state') <- liftIO $ runStateT action state `catch` \e ->
                          runStateT (handler e) state
    put state'
    return result

