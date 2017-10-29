{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Exception
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy

import Batchd.Core.Common.Localize
import Batchd.Client.Types
import Batchd.Client.CmdLine
import Batchd.Client.Config
import Batchd.Client.Monad
import Batchd.Client.Http
import Batchd.Client.Shell

main :: IO ()
main = realMain `catch` errorHandler

errorHandler :: ClientException -> IO ()
errorHandler (ClientException e) = TLIO.putStrLn =<< (__f "Error: {}" (Single e))

realMain :: IO ()
realMain = do
  setupTranslations translationPolicy
  opts <- getCmdArgs
  -- print opts

  -- manager <- makeClientManager opts
  cfg <- loadClientConfig
  let state = ClientState opts cfg Nothing Nothing Nothing Nothing

  runClient state $ commandHandler

