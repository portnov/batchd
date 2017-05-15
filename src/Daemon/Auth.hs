{-# LANGUAGE OverloadedStrings #-}
module Daemon.Auth where

import Control.Monad
import Control.Monad.Reader
import Control.Exception
import Data.Maybe
import Data.Int
import qualified Data.Vault.Lazy as V
import Database.Persist
import Database.Persist.Sql as Sql
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

---------- Users manipulation --------------------

createUserDb :: String -> String -> String -> DB (Key User)
createUserDb name password staticSalt = do
  dynamicSalt <- liftIO randomSalt
  let hash = calcHash password dynamicSalt staticSalt
  let user = User name hash dynamicSalt
  insert user

createSuperUserDb :: String -> String -> String -> DB (Key User)
createSuperUserDb name password staticSalt = do
  userKey <- createUserDb name password staticSalt
  let perm = UserPermission name SuperUser Nothing
  insert perm
  return userKey

createUser :: GlobalConfig -> String -> String -> IO Bool
createUser gcfg name password = do
  pool <- getPool gcfg
  let staticSalt = dbcStaticSalt gcfg
  res <- runDBIO gcfg pool (createUserDb name password staticSalt)
  case res of
    Left _ -> return False
    Right _ -> return True

createSuperUser :: GlobalConfig -> String -> String -> IO Bool
createSuperUser gcfg name password = do
  pool <- getPool gcfg
  let staticSalt = dbcStaticSalt gcfg
  res <- runDBIO gcfg pool (createSuperUserDb name password staticSalt)
  case res of
    Left _ -> return False
    Right _ -> return True

createPermission :: String -> UserPermission -> DB (Key UserPermission)
createPermission name perm = do
  let perm' = perm {userPermissionUserName = name}
  insert perm'

getPermissions :: String -> DB [UserPermission]
getPermissions name = do
  res <- selectList [UserPermissionUserName ==. name] []
  return $ map entityVal res

deletePermission :: Int64 -> String -> DB ()
deletePermission id name = do
  let pid = UserPermissionKey (SqlBackendKey id)
  mbPerm <- get pid
  case mbPerm of
    Nothing -> throwR $ UnknownError "Permission does not exist"
    Just perm -> do
      if userPermissionUserName perm == name
        then delete pid
        else throwR $ UnknownError "Permission does not belong to specified user"

getUsers :: DB [String]
getUsers = do
  res <- selectList [] []
  return $ map (userName . entityVal) res

------------------ Check user -----------------------

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

------------------ Authentication ---------------------

basicAuth :: GlobalConfig -> Middleware
basicAuth gcfg app req sendResponse =
  let settings = "batchd" :: HA.AuthSettings
      username = case (lookup hAuthorization $ requestHeaders req) >>= HA.extractBasicAuth of
                   Nothing -> "anonymous"
                   Just (name,_) -> bstrToString name
      req' = req {vault = V.insert usernameKey username $ vault req}
  in  case getAuthUserRq req of
        Nothing -> HA.basicAuth (checkUser gcfg) settings app req' sendResponse
        Just _ -> app req sendResponse

headerAuth :: GlobalConfig -> Middleware
headerAuth gcfg app req sendResponse = do
  -- liftIO $ putStrLn $ "X-Auth-User: " ++ show req
  case lookup "X-Auth-User" (requestHeaders req) of
    Nothing -> do
        app req sendResponse
--       case getAuthUserRq req of
--         Nothing -> sendResponse $ responseLBS status401 [] "User name not provided"
--         Just _ -> app req sendResponse
    Just name -> do
      let username = bstrToString name
          req' = req {vault = V.insert usernameKey username $ vault req}
      ok <- liftIO $ checkUserExists gcfg name
      if ok
        then app req' sendResponse
        else sendResponse $ responseLBS status401 [] "Specified user does not exist"

-- authentication :: GlobalConfig -> Middleware
-- authentication gcfg =
--   let m1 = if dbcEnableHeaderAuth gcfg
--              then headerAuth gcfg
--              else id
--       m2 = if dbcEnableBasicAuth gcfg
--              then basicAuth gcfg
--              else id
--   in m2 . m1

------------ Get current user ------------

getAuthUserRq :: Request -> Maybe String
getAuthUserRq req = V.lookup usernameKey $ vault req

getAuthUserName :: Action (Maybe String)
getAuthUserName = do
  rq <- Scotty.request
  return $ getAuthUserRq rq

getAuthUser :: Action User
getAuthUser = do
  mbName <- getAuthUserName
  case mbName of
    Nothing -> throw $ InsufficientRights "user has to be authenticated"
    Just name -> do
        mbUser <- runDBA $ get (UserKey name)
        case mbUser of
          Nothing -> throw $ UnknownError "user does not exist"
          Just user -> return user

----------- Check user rights ---------------

isSuperUser :: String -> DB Bool
isSuperUser name = do
  res <- selectList [UserPermissionUserName ==. name, UserPermissionPermission ==. SuperUser] []
  return $ not $ null res

checkSuperUser :: Action ()
checkSuperUser = do
  user <- getAuthUser
  ok <- runDBA $ isSuperUser (userName user)
  when (not ok) $ do
    throw $ InsufficientRights "user has to be superuser"

hasPermission :: String -> Permission -> String -> DB Bool
hasPermission name perm qname = do
  super <- isSuperUser name
  if super
    then return True
    else do
      exact <- selectList [UserPermissionUserName ==. name, UserPermissionPermission ==. perm, UserPermissionQueueName ==. Just qname] []
      if not (null exact)
        then return True
        else do
          any <- selectList [UserPermissionUserName ==. name, UserPermissionPermission ==. perm, UserPermissionQueueName ==. Nothing] []
          return $ not $ null any

hasPermissionToList :: String -> Permission -> DB Bool
hasPermissionToList name perm = do
  super <- isSuperUser name
  if super
    then return True
    else do
         any <- selectList [UserPermissionUserName ==. name, UserPermissionPermission ==. perm, UserPermissionQueueName ==. Nothing] []
         return $ not $ null any

checkPermission :: String -> Permission -> String -> Action ()
checkPermission message perm qname = do
  user <- getAuthUser
  ok <- runDBA $ hasPermission (userName user) perm qname
  when (not ok) $ do
    throw $ InsufficientRights message

checkPermissionToList :: String -> Permission -> Action ()
checkPermissionToList message perm = do
  user <- getAuthUser
  ok <- runDBA $ hasPermissionToList (userName user) perm
  when (not ok) $ do
    throw $ InsufficientRights message
