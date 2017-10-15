{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Daemon.Auth where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import Data.Int
import qualified Data.Vault.Lazy as V
import Database.Persist
import Database.Persist.Sql as Sql hiding (Single)
import qualified Data.ByteString as B
import Network.HTTP.Types (status401, hAuthorization)
import Network.Wai
import qualified Network.Wai.Middleware.HttpAuth as HA
import qualified Web.Scotty.Trans as Scotty
import System.Log.Heavy (Logger)
import Data.Text.Format.Heavy

import System.IO.Unsafe (unsafePerformIO)

import Common.Types
import Common.Data
import Daemon.Types
import Daemon.Crypto
import Daemon.Database
import Daemon.Logging

-- | WAI Request vault key for currently authenticated user name
usernameKey :: V.Key String
usernameKey = unsafePerformIO V.newKey
{-# NOINLINE usernameKey #-}

---------- Users manipulation --------------------

-- | Create user
createUserDb :: String -- ^ User name
             -> String -- ^ Password
             -> String -- ^ Static part of salt
             -> DB (Key User)
createUserDb name password staticSalt = do
  dynamicSalt <- liftIO randomSalt
  let hash = calcHash password dynamicSalt staticSalt
  let user = User name hash dynamicSalt
  insert user

-- | Create superuser
createSuperUserDb :: String -- ^ User name
                  -> String -- ^ Password
                  -> String -- ^ Static part of salt
                  -> DB (Key User)
createSuperUserDb name password staticSalt = do
  userKey <- createUserDb name password staticSalt
  let perm = UserPermission name SuperUser Nothing Nothing Nothing
  insert perm
  return userKey

-- | Create user
createUser :: GlobalConfig
           -> Logger
           -> String -- ^ User name
           -> String -- ^ Password
           -> IO Bool
createUser gcfg logger name password = do
  pool <- getPool gcfg logger
  let staticSalt = authStaticSalt $ dbcAuth gcfg
  res <- runDBIO gcfg pool logger (createUserDb name password staticSalt)
  case res of
    Left _ -> return False
    Right _ -> return True

-- | Create superuser
createSuperUser :: GlobalConfig
                -> Logger
                -> String -- ^ User name
                -> String -- ^ Password
                -> IO Bool
createSuperUser gcfg logger name password = do
  pool <- getPool gcfg logger
  let staticSalt = authStaticSalt $ dbcAuth gcfg
  res <- runDBIO gcfg pool logger (createSuperUserDb name password staticSalt)
  case res of
    Left _ -> return False
    Right _ -> return True

-- | Create permission
createPermission :: String -- ^ User name
                 -> UserPermission
                 -> DB (Key UserPermission)
createPermission name perm = do
  let perm' = perm {userPermissionUserName = name}
  insert perm'

-- | Get list of permissions and their IDs for specified user
getPermissions :: String -- ^ User name
               -> DB [(Int64, UserPermission)]
getPermissions name = do
  res <- selectList [UserPermissionUserName ==. name] []
  let getKey (UserPermissionKey (SqlBackendKey id)) = id
  return [(getKey (entityKey e), entityVal e) | e <- res]

-- | Delete permission. Fail if specified permission does not belong to specified user.
deletePermission :: Int64 -- ^ Permission ID
                 -> String -- ^ User name
                 -> DB ()
deletePermission id name = do
  let pid = UserPermissionKey (SqlBackendKey id)
  mbPerm <- get pid
  case mbPerm of
    Nothing -> throwR $ UnknownError "Permission does not exist"
    Just perm -> do
      if userPermissionUserName perm == name
        then delete pid
        else throwR $ UnknownError "Permission does not belong to specified user"

-- | Change user password
changePassword :: String -- ^ User name
               -> String -- ^ New password
               -> String -- ^ Static part of salt
               -> DB ()
changePassword name password staticSalt = do
  dynamicSalt <- liftIO randomSalt
  let hash = calcHash password dynamicSalt staticSalt
      key = UserKey name
  update key [UserPwdHash =. hash, UserSalt =. dynamicSalt]

-- | Get list of all users
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

checkUser :: GlobalConfig -> Logger -> (B.ByteString -> B.ByteString -> IO Bool)
checkUser gcfg logger nameBstr passwordBstr = do
  pool <- getPool gcfg logger
  let name = bstrToString nameBstr
      password = bstrToString passwordBstr
      salt = authStaticSalt $ dbcAuth gcfg
  res <- runDBIO gcfg pool logger (checkUserDb name password salt)
  case res of
    Left _ -> return False
    Right r -> return r

checkUserExists :: GlobalConfig -> Logger -> B.ByteString -> IO Bool
checkUserExists gcfg logger nameBstr = do
  pool <- getPool gcfg logger
  let name = bstrToString nameBstr
  res <- runDBIO gcfg pool logger (checkUserExistsDb name)
  case res of
    Left _ -> return False
    Right r -> return r

------------------ Authentication ---------------------

extractBasicUser :: Request -> String
extractBasicUser req =
  case (lookup hAuthorization $ requestHeaders req) >>= HA.extractBasicAuth of
    Nothing -> "anonymous"
    Just (name,_) -> bstrToString name

-- | Returns true if this is OPTIONS request for @/@
isRootOptions :: Request -> Bool
isRootOptions rq = requestMethod rq == "OPTIONS" && pathInfo rq == []

-- | HTTP basic auth middleware
basicAuth :: GlobalConfig -> Logger -> Middleware
basicAuth gcfg logger app req sendResponse =
  let settings = "batchd" :: HA.AuthSettings
      username = extractBasicUser req
      -- put user name extracted from header to vault
      req' = req {vault = V.insert usernameKey username $ vault req}
  in  if isRootOptions req
        then app req sendResponse -- OPTIONS / is available without auth
        else case getAuthUserRq req of
                 Nothing -> do
                     infoIO logger $(here) "Will try to authenticate user with basic auth: {}" (Single username)
                     HA.basicAuth (checkUser gcfg logger) settings app req' sendResponse
                 Just _ -> app req sendResponse

-- | Authentication by X-Auth-User HTTP header
headerAuth :: GlobalConfig -> Logger -> Middleware
headerAuth gcfg logger app req sendResponse = do
  -- liftIO $ putStrLn $ "X-Auth-User: " ++ show req
  if isRootOptions req
    then app req sendResponse -- OPTIONS / is available without auth
    else case lookup "X-Auth-User" (requestHeaders req) of
            Nothing -> do
                infoIO logger $(here) "No X-Auth-User header" ()
                app req sendResponse
        --       case getAuthUserRq req of
        --         Nothing -> sendResponse $ responseLBS status401 [] "User name not provided"
        --         Just _ -> app req sendResponse
            Just name -> do
              let username = bstrToString name
                  -- put user name extracted from header to vault
                  req' = req {vault = V.insert usernameKey username $ vault req}
              ok <- liftIO $ checkUserExists gcfg logger name
              infoIO logger $(here) "User from X-AUth-User header authenticated: {}" (Single username)
              if ok
                then app req' sendResponse
                else sendResponse $ responseLBS status401 [] "Specified user does not exist"

-- | Unconditional authentication
noAuth :: GlobalConfig -> Logger -> Middleware
noAuth gcfg logger app req sendResponse = do
  let username = case lookup "X-Auth-User" (requestHeaders req) of
                   Just n -> bstrToString n
                   Nothing -> extractBasicUser req
      -- if by some condition username appeared in header, put it to vault.
      -- otherwise user name will be anonymous.
      req' = req {vault = V.insert usernameKey username $ vault req}
  infoIO logger $(here) "Authentication is disabled. User treated as superuser: {}" (Single username)
  app req' sendResponse

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

-- | Get user name from WAI Request vault
getAuthUserRq :: Request -> Maybe String
getAuthUserRq req = V.lookup usernameKey $ vault req

-- | Get currently authenticated user name.
-- Nothing means user was not authenticated.
getAuthUserName :: Action (Maybe String)
getAuthUserName = do
  rq <- Scotty.request
  return $ getAuthUserRq rq

-- | Get currently authenticated user.
-- Fail if user was not authenticated.
getAuthUser :: Action User
getAuthUser = do
  mbName <- getAuthUserName
  case mbName of
    Nothing -> Scotty.raise $ InsufficientRights "user has to be authenticated"
    Just name -> do
        mbUser <- runDBA $ get (UserKey name)
        case mbUser of
          Nothing -> Scotty.raise $ UnknownError "user does not exist"
          Just user -> return user

isAuthDisabled :: AuthMode -> Bool
isAuthDisabled AuthDisabled = True
isAuthDisabled _ = False

----------- Check user rights ---------------

-- | Check if user has SuperUser permission
isSuperUser :: String -> DB Bool
isSuperUser name = do
  res <- selectList [UserPermissionUserName ==. name, UserPermissionPermission ==. SuperUser] []
  return $ not $ null res

-- | Check if user is superuser. Fail otherwise.
checkSuperUser :: Action ()
checkSuperUser = do
  cfg <- askConfigA
  when (not $ isAuthDisabled $ dbcAuth cfg) $ do
      user <- getAuthUser
      ok <- runDBA $ isSuperUser (userName user)
      when (not ok) $ do
        Scotty.raise $ InsufficientRights "user has to be superuser"

-- | Check that user is authenticated. Fail otherwise.
checkUserAuthenticated :: Action ()
checkUserAuthenticated = do
  _ <- getAuthUser
  return ()

-- | Check if user has specified permission
hasPermission :: String -- ^ User name
              -> Permission
              -> String -- ^ Queue name
              -> DB Bool
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

-- | Special constant for default host of the queue
defaultHostOfQueue :: String
defaultHostOfQueue = "__default__"

-- | Check if user has permissions to create jobs in certain queue
hasCreatePermission :: String -- ^ User name
                    -> String -- ^ Queue name
                    -> Maybe String -- ^ Just type name, or Nothing if you want to check that user has permission
                                    -- to create jobs of at least some type
                    -> Maybe String -- ^ Just host name, or Nothing if you want to check that user has permission
                                    -- to create jobs on at least some host. Use special @__default__@ value
                                    -- for default host of queue.
                    -> DB Bool
hasCreatePermission name qname mbTypename mbHostname = do
  super <- isSuperUser name
  if super
    then return True
    else do
      let byUser  = [UserPermissionUserName ==. name, UserPermissionPermission ==. CreateJobs]
          byQueue = [UserPermissionQueueName ==. Nothing] ||. [UserPermissionQueueName ==. Just qname]
          byType  = case mbTypename of
                      Nothing -> []
                      Just typename -> [UserPermissionTypeName ==. Nothing] ||. [UserPermissionTypeName ==. Just typename]
          byHost  = case mbHostname of
                      Nothing -> []
                      Just hostname -> [UserPermissionHostName ==. Nothing] ||. [UserPermissionHostName ==. Just hostname]
      let filtr = byUser ++ byQueue ++ byType ++ byHost
      res <- selectList filtr []
      return $ not $ null res

-- | List names of hosts where user can create jobs of specified type in certain queue.
-- Returns Nothing if user can create jobs on any host.
listAllowedHosts :: String -- ^ User name
                 -> String -- ^ Queue name
                 -> String -- ^ Job type name
                 -> DB (Maybe [String])
listAllowedHosts name qname typename = do
  super <- isSuperUser name
  if super
    then return Nothing
    else do
      let byUser  = [UserPermissionUserName ==. name, UserPermissionPermission ==. CreateJobs]
          byQueue = [UserPermissionQueueName ==. Nothing] ||. [UserPermissionQueueName ==. Just qname]
          byType  = [UserPermissionTypeName ==. Nothing] ||. [UserPermissionTypeName ==. Just typename]
          fltr = byUser ++ byQueue ++ byType
      qryResult <- selectList fltr []
      let mbHosts = map (userPermissionHostName . entityVal) qryResult
          result = if any isNothing mbHosts -- in this case user can create jobs on any host
                     then Nothing
                     else Just $ map fromJust mbHosts
      return result

-- | Check if user has permission to the full list of objects.
hasPermissionToList :: String     -- ^ User name
                    -> Permission
                    -> DB Bool
hasPermissionToList name perm = do
  super <- isSuperUser name
  if super
    then return True
    else do
         any <- selectList [UserPermissionUserName ==. name, UserPermissionPermission ==. perm, UserPermissionQueueName ==. Nothing] []
         return $ not $ null any

-- | Check that user has specified permission. Fail otherwise.
checkPermission :: String      -- ^ Error message for case of insufficient privileges
                -> Permission
                -> String      -- ^ Queue name
                -> Action ()
checkPermission message perm qname = do
  cfg <- askConfigA
  when (not $ isAuthDisabled $ dbcAuth cfg) $ do
      user <- getAuthUser
      ok <- runDBA $ hasPermission (userName user) perm qname
      when (not ok) $ do
        Scotty.raise $ InsufficientRights message

-- | Check if user can create jobs in given conditions. Fail otherwise.
checkCanCreateJobs :: String -- ^ Queue name
                   -> String -- ^ Job type name
                   -> String -- ^ Host name. Use @__default__@ for default host of queue.
                   -> Action ()
checkCanCreateJobs qname typename hostname = do
  cfg <- askConfigA
  when (not $ isAuthDisabled $ dbcAuth cfg) $ do
      user <- getAuthUser
      ok <- runDBA $ hasCreatePermission (userName user) qname (Just typename) (Just hostname)
      when (not ok) $ do
        Scotty.raise $ InsufficientRights "create jobs"

-- | Check that user has permission to full list of objects. Fail otherwise.
checkPermissionToList :: String     -- ^ Error message for case of insufficient privileges
                      -> Permission
                      -> Action ()
checkPermissionToList message perm = do
  cfg <- askConfigA
  when (not $ isAuthDisabled $ dbcAuth cfg) $ do
      user <- getAuthUser
      ok <- runDBA $ hasPermissionToList (userName user) perm
      when (not ok) $ do
        Scotty.raise $ InsufficientRights message

