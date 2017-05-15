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
  CreateSuperuser
  deriving (Show, Data, Typeable)

admin :: Admin
admin = modes [CreateSuperuser]
          &= program "batchd-admin"

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
  cmd <- cmdArgs admin
  cfgR <- loadGlobalConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      pwd1 <- getPassword "Password: "
      pwd2 <- getPassword "Password again: "
      if pwd1 /= pwd2
        then fail "passwords do not match"
        else do
             createUser cfg "root" pwd1
             return ()

