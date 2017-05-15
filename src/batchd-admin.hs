{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import System.Console.CmdArgs
import Data.Generics hiding (Generic)
import System.IO

import Common.Types
import Common.Config
import Daemon.Database
import Daemon.Auth

data Admin =
  CreateSuperuser {userName :: String}
  deriving (Show, Data, Typeable)

createSuperuser :: Admin
createSuperuser = CreateSuperuser {
    userName = "root" &= typ "NAME" &= argPos 0
  } &= help "create superuser"

getPassword :: String -> IO String
getPassword prompt = do
  putStr prompt
  hFlush stdout
  pass <- withEcho False getLine
  putChar '\n'
  return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

main :: IO ()
main = do
  let mode = cmdArgsMode $ modes [createSuperuser] &= program "batchd-admin"
  cmd <- cmdArgsRun mode
  cfgR <- loadGlobalConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      pwd1 <- getPassword "Password: "
      pwd2 <- getPassword "Password again: "
      if pwd1 /= pwd2
        then fail "passwords do not match"
        else do
             createSuperUser cfg (userName cmd) pwd1
             return ()

