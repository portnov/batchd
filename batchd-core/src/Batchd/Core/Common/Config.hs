{-# LANGUAGE OverloadedStrings #-}
-- | This module contains utility functions for locating and loading
-- configuration files.
module Batchd.Core.Common.Config where

import Control.Monad
import Control.Exception
import Data.Yaml
import System.FilePath
import System.Environment
import System.Directory
import System.IO

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize

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
             -> String    -- ^ Config file name (e.g. @"host.yaml"@)
             -> IO (Maybe FilePath)
locateConfig t name = do
  paths <- getConfigDirs t
  rs <- forM paths $ \path -> do
          let file = path </> name
          ex <- doesFileExist file
          return $ if ex then [file] else []
  case concat rs of
    [] -> return Nothing
    (result:_) -> return $ Just result

-- | Load configuration file of certain type.
loadConfig :: FromJSON config
           => String                    -- ^ Config type
           -> String                    -- ^ Config file name without extension (@"host"@)
           -> (ParseException -> Error) -- ^ Wrap YAML parsing error. This is usually one of @Error@ constructors.
           -> IO (Either Error config)
loadConfig t name exc = do
  mbPath <- locateConfig t (name ++ ".yaml")
  case mbPath of
    Nothing -> return $ Left $ FileNotExists (name ++ ".yaml")
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> return $ Left $ exc err
        Right cfg -> return $ Right cfg

-- | Load config file of host controller.
loadHostControllerConfig :: FromJSON config => String -> IO (Either Error config)
loadHostControllerConfig name = loadConfig "controllers" name InvalidHostControllerConfig

-- | Load host config file
loadHost :: String -> IO (Either Error Host)
loadHost name = loadConfig "hosts" name InvalidHost

-- | Load job type config file
loadTemplate :: String -> IO (Either Error JobType)
loadTemplate name = loadConfig "jobtypes" name InvalidJobType

-- | Load global config file (@"batchd.yaml"@)
loadGlobalConfig :: IO (Either Error GlobalConfig)
loadGlobalConfig = do
  mbPath <- locateConfig "" "batchd.yaml"
  case mbPath of
    Nothing -> return $ Left $ FileNotExists "batchd.yaml"
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> return $ Left $ InvalidDbCfg err
        Right cfg -> do
          return $ Right cfg

-- | Read password from stdout.
getPassword :: String    -- ^ Prompt
           -> IO String
getPassword prompt = do
  putStr prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

-- | Ask password from stdout twice.
-- Make sure that entered passwords are equal.
getPassword2 :: IO String
getPassword2 = do
  pwd1 <- getPassword =<< (__s "Password: ")
  pwd2 <- getPassword =<< (__s "Password again: ")
  if pwd1 /= pwd2
    then fail =<< (__s "passwords do not match")
    else return pwd1

-- | Execute IO actions with enabled\/disabled terminal echo,
-- and then return echo state to previous.
withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

