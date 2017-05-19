{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Monad where

import Control.Monad
import Control.Monad.State
import Control.Exception
import Data.Maybe
import Network.HTTP.Client

import Common.Types
import Common.Config (getPassword)
import Client.Config
import Client.Types
import Client.CmdLine

data ClientState = ClientState {
    csCmdline :: Batch,
    csConfig :: ClientConfig,
    csCredentials :: Maybe Credentials,
    csAuthMethods :: Maybe [AuthMethod],
    csManager :: Manager
  }

type Client a = StateT ClientState IO a

runClient :: ClientState -> Client a -> IO a
runClient st action = evalStateT action st

getBaseUrl :: Client String
getBaseUrl = do
  cfg <- gets csConfig
  opts <- gets csCmdline
  liftIO $ getManagerUrl (managerUrl opts) cfg

throwC :: String -> Client a
throwC msg = lift $ throw $ ClientException msg

