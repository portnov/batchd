
module Common.Config where

import Control.Monad
import Data.Yaml
import System.FilePath
import System.Environment
import System.Directory

import Common.CommonTypes

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
        Right cfg -> return $ Right cfg

