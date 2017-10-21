{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Client.Monad where

import Control.Monad.State
import Control.Exception
import qualified Data.Text.Lazy as TL
import Network.HTTP.Client
import Text.Localize
import Text.Localize.IO

import Common.Types
import Client.Config
import Client.Types
import Client.CmdLine

data ClientState = ClientState {
    csCmdline :: CmdLine,
    csConfig :: ClientConfig,
    csCredentials :: Maybe Credentials,
    csAuthMethods :: Maybe [AuthMethod],
    csManager :: Manager
  }

type Client a = StateT ClientState IO a

instance Localized (StateT ClientState IO) where
  getLanguage = lift getLanguage
  getTranslations = lift getTranslations
  getContext = lift getContext

runClient :: ClientState -> Client a -> IO a
runClient st action = evalStateT action st

getBaseUrl :: Client String
getBaseUrl = do
  cfg <- gets csConfig
  opts <- gets csCmdline
  liftIO $ getManagerUrl opts cfg

throwC :: TL.Text -> Client a
throwC msg = lift $ throw $ ClientException msg

wrapClient :: (forall a. IO a -> IO a) -> Client b -> Client b
wrapClient wrapper actions = do
  state <- get
  (result, state') <- liftIO $ wrapper $ do
                        runStateT actions state
  put state'
  return result

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

