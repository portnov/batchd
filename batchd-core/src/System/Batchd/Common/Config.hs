{-# LANGUAGE OverloadedStrings #-}
module System.Batchd.Common.Config where

import Control.Monad
import Control.Exception
import Data.Yaml
import System.FilePath
import System.Environment
import System.Directory
import System.IO

import System.Batchd.Common.Types
import System.Batchd.Common.Localize

getConfigDirs :: String -> IO [FilePath]
getConfigDirs t = do
  home <- getEnv "HOME"
  let homeCfg = home </> ".config" </> "batchd" </> t
  homeExists <- doesDirectoryExist homeCfg
  let etc = "/etc" </> "batchd" </> t
  etcExists <- doesDirectoryExist etc
  return $ (if homeExists then [homeCfg] else []) ++
           (if etcExists  then [etc] else [])

locateConfig :: String -> String -> IO (Maybe FilePath)
locateConfig t name = do
  paths <- getConfigDirs t
  rs <- forM paths $ \path -> do
          let file = path </> name
          ex <- doesFileExist file
          return $ if ex then [file] else []
  case concat rs of
    [] -> return Nothing
    (result:_) -> return $ Just result

loadConfig :: FromJSON config => String -> String -> (ParseException -> Error) -> IO (Either Error config)
loadConfig t name exc = do
  mbPath <- locateConfig t (name ++ ".yaml")
  case mbPath of
    Nothing -> return $ Left $ FileNotExists (name ++ ".yaml")
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> return $ Left $ exc err
        Right cfg -> return $ Right cfg

loadHostControllerConfig :: FromJSON config => String -> IO (Either Error config)
loadHostControllerConfig name = loadConfig "controllers" name InvalidHostControllerConfig

loadHost :: String -> IO (Either Error Host)
loadHost name = loadConfig "hosts" name InvalidHost

loadTemplate :: String -> IO (Either Error JobType)
loadTemplate name = loadConfig "jobtypes" name InvalidJobType

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

getPassword :: String -> IO String
getPassword prompt = do
  putStr prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

getPassword2 :: IO String
getPassword2 = do
  pwd1 <- getPassword =<< (__s "Password: ")
  pwd2 <- getPassword =<< (__s "Password again: ")
  if pwd1 /= pwd2
    then fail =<< (__s "passwords do not match")
    else return pwd1

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

