{-# LANGUAGE OverloadedStrings #-}
-- | This module contains utility functions for locating and loading
-- configuration files.
module Batchd.Core.Common.Config where

import Control.Monad
import Data.Yaml
import qualified Data.Text as T
import System.FilePath
import System.Environment
import System.Directory

import Batchd.Core.Common.Types

-- | Get list of directories in which to look up for configs
-- of certain type. Example:
-- @getConfigDirs "hosts" == ["/home/user/.config/batchd/hosts", "/etc/batchd/hosts"]@
getConfigDirs :: String -> IO [FilePath]
getConfigDirs t = do
  home <- getEnv "HOME"
  let homeCfg = home </> ".config" </> "batchd" </> t
  homeExists <- doesDirectoryExist homeCfg
  let etc = "/etc" </> "batchd" </> t
  etcExists <- doesDirectoryExist etc
  return $ (if homeExists then [homeCfg] else []) ++
           (if etcExists  then [etc] else [])

-- | Locate config file of certain type.
-- Returns Nothing if no files found.
locateConfig :: String    -- ^ Config type
             -> T.Text    -- ^ Config file name (e.g. @"host.yaml"@)
             -> IO (Maybe FilePath)
locateConfig t name = do
  paths <- getConfigDirs t
  let nameStr = T.unpack name
  rs <- forM paths $ \path -> do
          let file = path </> nameStr
          ex <- doesFileExist file
          return $ if ex then [file] else []
  case concat rs of
    [] -> return Nothing
    (result:_) -> return $ Just result

-- | Load configuration file of certain type.
loadConfig :: FromJSON config
           => String                    -- ^ Config type
           -> T.Text                    -- ^ Config file name without extension (@"host"@)
           -> (ParseException -> Error) -- ^ Wrap YAML parsing error. This is usually one of @Error@ constructors.
           -> IO (Either Error config)
loadConfig t name exc = do
  mbPath <- locateConfig t (name <> ".yaml")
  case mbPath of
    Nothing -> return $ Left $ FileNotExists (T.unpack name ++ ".yaml")
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> return $ Left $ exc err
        Right cfg -> return $ Right cfg

-- | Load config file of host controller.
loadHostControllerConfig :: FromJSON config => T.Text -> IO (Either Error config)
loadHostControllerConfig name = loadConfig "controllers" name InvalidHostControllerConfig

-- | Load host config file
loadHost :: T.Text -> IO (Either Error Host)
loadHost name = loadConfig "hosts" name InvalidHost

