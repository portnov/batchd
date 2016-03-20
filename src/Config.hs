
module Config where

import Control.Monad
import Data.Aeson
import Data.Yaml
import System.FilePath
import System.Environment
import System.Directory

import CommonTypes

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

loadHost :: String -> IO (Either Error Host)
loadHost name = do
  mbPath <- locateConfig "hosts" (name ++ ".yaml")
  case mbPath of
    Nothing -> return $ Left FileNotExists
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> return $ Left $ InvalidHost err
        Right host -> return $ Right host

loadTemplate :: String -> IO (Either Error JobType)
loadTemplate name = do
  mbPath <- locateConfig "jobtypes" (name ++ ".yaml")
  case mbPath of
    Nothing -> return $ Left FileNotExists
    Just path -> do
      r <- decodeFileEither path
      case r of
        Left err -> return $ Left $ InvalidJobType err
        Right jt -> return $ Right jt

