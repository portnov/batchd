{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Exception
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy

import Common.Localize
import Client.Types
import Client.CmdLine
import Client.Config
import Client.Monad
import Client.Http
import Client.Shell

main :: IO ()
main = realMain `catch` errorHandler

errorHandler :: ClientException -> IO ()
errorHandler (ClientException e) = TLIO.putStrLn =<< (__f "Error: {}" (Single e))

realMain :: IO ()
realMain = do
  setupTranslations translationPolicy
  opts <- getCmdArgs
  -- print opts

  manager <- makeClientManager opts
  cfg <- loadClientConfig
  let state = ClientState opts cfg Nothing Nothing manager

  runClient state $ commandHandler

