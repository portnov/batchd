{-# LANGUAGE OverloadedStrings #-}
module Daemon.Auth where

import Control.Monad.Reader
import Data.Maybe
import qualified Data.Vault.Lazy as V
import Database.Persist
import qualified Data.ByteString as B
import Network.HTTP.Types (status401, hAuthorization)
import Network.Wai
import qualified Network.Wai.Middleware.HttpAuth as HA
import qualified Web.Scotty.Trans as Scotty

import System.IO.Unsafe (unsafePerformIO)

import Common.Types
import Common.Data
import Daemon.Types
import Daemon.Crypto
import Daemon.Database

usernameKey :: V.Key String
usernameKey = unsafePerformIO V.newKey
{-# NOINLINE usernameKey #-}

createUserDb :: String -> String -> String -> DB (Key User)
createUserDb name password staticSalt = do
  dynamicSalt <- liftIO randomSalt
  let hash = calcHash password dynamicSalt staticSalt
  let user = User name hash dynamicSalt
  insert user

createUser :: GlobalConfig -> String -> String -> IO Bool
createUser gcfg name password = do
  pool <- getPool gcfg
  let staticSalt = dbcStaticSalt gcfg
  res <- runDBIO gcfg pool (createUserDb name password staticSalt)
  case res of
    Left _ -> return False
    Right _ -> return True

checkUserDb :: String -> String -> String -> DB Bool
checkUserDb name password staticSalt = do
  mbUser <- get (UserKey name)
  case mbUser of
    Nothing -> return False
    Just user -> do
        let hash = calcHash password (userSalt user) staticSalt
        return $ hash == userPwdHash user

checkUserExistsDb :: String -> DB Bool
checkUserExistsDb name = do
  mbUser <- get (UserKey name)
  return $ isJust mbUser

checkUser :: GlobalConfig -> (B.ByteString -> B.ByteString -> IO Bool)
checkUser gcfg nameBstr passwordBstr = do
  pool <- getPool gcfg
  let name = bstrToString nameBstr
      password = bstrToString passwordBstr
      salt = dbcStaticSalt gcfg
  res <- runDBIO gcfg pool (checkUserDb name password salt)
  case res of
    Left _ -> return False
    Right r -> return r

checkUserExists :: GlobalConfig -> B.ByteString -> IO Bool
checkUserExists gcfg nameBstr = do
  pool <- getPool gcfg
  let name = bstrToString nameBstr
  res <- runDBIO gcfg pool (checkUserExistsDb name)
  case res of
    Left _ -> return False
    Right r -> return r

basicAuth :: GlobalConfig -> Middleware
basicAuth gcfg app req sendResponse =
  let settings = "batchd" :: HA.AuthSettings
      username = case (lookup hAuthorization $ requestHeaders req) >>= HA.extractBasicAuth of
                   Nothing -> "anonymous"
                   Just (name,_) -> bstrToString name
      req' = req {vault = V.insert usernameKey username $ vault req}
  in  HA.basicAuth (checkUser gcfg) settings app req' sendResponse

getAuthUserRq :: Request -> Maybe String
getAuthUserRq req = V.lookup usernameKey $ vault req

getAuthUserName :: Action (Maybe String)
getAuthUserName = do
  rq <- Scotty.request
  return $ getAuthUserRq rq

headerAuth :: GlobalConfig -> Middleware
headerAuth gcfg app req sendResponse = do
  case lookup "X-Auth-User" (requestHeaders req) of
    Nothing -> do
      case getAuthUserRq req of
        Nothing -> sendResponse $ responseLBS status401 [] "User name not provided"
        Just _ -> app req sendResponse
    Just name -> do
      let username = bstrToString name
          req' = req {vault = V.insert usernameKey username $ vault req}
      ok <- liftIO $ checkUserExists gcfg name
      if ok
        then app req' sendResponse
        else sendResponse $ responseLBS status401 [] "Specified user does not exist"

authentication :: GlobalConfig -> Middleware
authentication gcfg =
  let m1 = if dbcEnableHeaderAuth gcfg
             then headerAuth gcfg
             else id
      m2 = if dbcEnableBasicAuth gcfg
             then basicAuth gcfg
             else id
  in m2 . m1

