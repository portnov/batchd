{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdArgs
import Data.Generics hiding (Generic)

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

main :: IO ()
main = do
  let mode = cmdArgsMode $ modes [createSuperuser] &= program "batchd-admin"
  cmd <- cmdArgsRun mode
  cfgR <- loadGlobalConfig
  case cfgR of
    Left err -> fail $ show err
    Right cfg -> do
      password <- getPassword2
      createSuperUser cfg (userName cmd) password
      return ()

