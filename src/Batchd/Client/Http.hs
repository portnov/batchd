{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Batchd.Client.Http where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Format.Heavy
import Data.Default
import Data.List
import Data.Aeson as Aeson
import Data.X509.CertificateStore
import Network.HTTP.Client
import Network.HTTP.Types.Status
import Network.HTTP.Client.TLS
import Network.TLS as TLS
import Network.TLS.Extra.Cipher
import Network.Connection

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import Batchd.Core.Common.Config (getPassword)
import Batchd.Client.Types as C
import Batchd.Client.Config
import Batchd.Client.CmdLine
import Batchd.Client.Monad
import Batchd.Client.Logging

-- | Create a Manager - a structure which contains all required information to
-- maintain HTTP connection, over TLS if required.
makeClientManager :: CmdLine -> Client Manager
makeClientManager opts = do
    cfg <- liftIO loadClientConfig
    url <- liftIO $ getManagerUrl opts cfg
    if "https" `isPrefixOf` url
      then mkMngr (ccDisableServerCertificateCheck cfg) (ccCertificate cfg) (ccKey cfg) (ccCertificate cfg)
      else liftIO $ newManager defaultManagerSettings -- for HTTP everything is easy
  where
    mkMngr :: Bool -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> Client Manager
    mkMngr disableServerCertCheck mbCertFile mbKeyFile mbCaFile = do
     when disableServerCertCheck $ do
         verbose (__ "Server certificate check disabled") ()
     -- load Credentials from certificate and private key file (.pem and .key usually)
     creds <- case (mbCertFile, mbKeyFile) of
                (Just certFile, Just keyFile) -> do
                    verbose (__ "Loading HTTPS certificate: {}, key: {}") (certFile, keyFile)
                    r <- liftIO $ credentialLoadX509 certFile keyFile
                    case r of
                      Right cr -> return $ Just cr
                      Left err -> fail =<< (__sf "Can't load certificate/key: {}" (Single $ show err))
                _ -> do
                     message (__ "Certificate or key file is not specified, try to access HTTPS without them") ()
                     return Nothing
     -- load trusted CA store if specified
     mbStore <- case mbCaFile of
                       Nothing -> return Nothing
                       Just caFile -> do
                          r <- liftIO $ readCertificateStore caFile
                          case r of
                            Just store -> return $ Just store
                            Nothing -> fail =<< (__s "cannot read specified CA store")
     let shared = case mbStore of
                    Nothing -> def
                    Just store -> def { sharedCAStore = store }
                
     let clientCertHook _ = return creds
         skip _ _ _ _ = do
                return []

         -- handlers for TLS protocol events on client side
         hooks =
            if disableServerCertCheck
              then def {
                         onCertificateRequest = clientCertHook, -- return loaded client certificate
                         onServerCertificate = skip
                       }
              else def { onCertificateRequest = clientCertHook }

         clientParams = (defaultParamsClient "" "")
                        { clientHooks = hooks,
                          clientUseServerNameIndication = True, -- SNI
                          clientShared = shared,
                          -- This is probably to be configurable
                          clientSupported = def { supportedCiphers = ciphersuite_all, supportedVersions = [TLS12] }
                        }
         tlsSettings = TLSSettings clientParams

     liftIO $ newManager $ mkManagerSettings tlsSettings Nothing

-- | Obtain list of authentication methods supported by server.
-- This is done by requesting OPTIONS /.
getAuthMethods :: Client [AuthMethod]
getAuthMethods = do
  mbMethods <- gets csAuthMethods
  case mbMethods of
    Just methods -> return methods
    Nothing -> do
      baseUrl <- getBaseUrl
      methods <- doOptions baseUrl
      modify $ \st -> st {csAuthMethods = Just methods}
      return methods

-- | Obtain user name and password if required
obtainCredentials :: Client C.Credentials
obtainCredentials = do
  cfg <- gets csConfig
  opts@(CmdLine o _) <- gets csCmdline
  methods <- getAuthMethods
  verbose (__ "Supported authentication methods: {}") (Single $ show methods)
  let needPassword = BasicAuth `elem` methods -- only basic auth of currently supported methods requires a password.
  name <- liftIO $ getUserName opts cfg
  pass <- if ccDisableAuth cfg || not needPassword
            then do
                 debug (__ "Password is not required") ()
                 return ""
            else do
                 mbPassword <- liftIO $ getConfigParam' (password o) "BATCH_PASSWORD" (ccPassword cfg)
                 case mbPassword of
                   Just p -> return p
                   Nothing -> liftIO $ getPassword =<< (__sf "{} password: " (Single name))
  let creds = (name, pass)
  -- remember credentials in client state
  modify $ \st -> st {csCredentials = Just creds}
  return creds

-- | Obain and cache user name and password, if required
getCredentials :: Client C.Credentials
getCredentials = do
  mbCreds <- gets csCredentials
  case mbCreds of
    Just creds -> return creds
    Nothing -> obtainCredentials

-- | Add authentication headers to HTTP request
applyAuth :: Request -> Client Request
applyAuth rq = do
  methods <- getAuthMethods
  -- liftIO $ print methods
  if BasicAuth `elem` methods
    then do
         (name, password) <- getCredentials
         return $ applyBasicAuth (stringToBstr name) (stringToBstr password) rq
    else if HeaderAuth `elem` methods
           then do
                (name, _) <- getCredentials
                return $ rq {requestHeaders = ("X-Auth-User", stringToBstr name) : requestHeaders rq}
           else return rq

handleStatus :: Response L.ByteString -> Client L.ByteString
handleStatus rs = do
  debug (__ "Received response: {}") (Single $ Shown rs)
  verbose (__ "Server response code is {} {}") (statusCode $ responseStatus rs, statusMessage $ responseStatus rs)
  if responseStatus rs == ok200
    then return $ responseBody rs
    else do
         liftIO $ throw $ ClientException $ TLE.decodeUtf8 $ responseBody rs

allowAny :: Request -> Response BodyReader -> IO ()
allowAny _ _ = return ()

getManager :: Client Manager
getManager = do
  mbManager <- gets csManager
  case mbManager of
    Just manager -> return manager
    Nothing -> do
               cmdline <- gets csCmdline
               manager <- makeClientManager cmdline
               modify $ \st -> st {csManager = Just manager}
               return manager

-- Generic HTTP request wrapper, useful mostly for debugging
doHttp :: Request -> Manager -> Client L.ByteString
doHttp request manager = do
  debug (__ "Sending request: {}") (Single $ Shown request)
  handleStatus =<< (liftIO $ httpLbs request manager)

-- PUT request
doPut :: ToJSON a => String -> a -> Client ()
doPut urlStr object = do
  manager <- getManager
  url <- liftIO $ parseUrlThrow urlStr
  let json = Aeson.encode object
  debug (__ "Request JSON: {}") (Single json)
  request <- applyAuth $ url {
                  method="PUT",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS json
                }
  doHttp request manager
  return ()

-- POST request
doPost :: ToJSON a => String -> a -> Client ()
doPost urlStr object = do
  manager <- getManager
  url <- liftIO $ parseUrlThrow urlStr
  let json = Aeson.encode object
  debug (__ "Request JSON: {}") (Single json)
  request <- applyAuth $ url {
                  method="POST",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS json
                }
  doHttp request manager
  return ()

-- DELETE request
doDelete :: String -> Client ()
doDelete urlStr = do
  manager <- getManager
  url <- liftIO $ parseUrlThrow urlStr
  request <- applyAuth $ url { method="DELETE",
                      checkResponse = allowAny
                    }
  doHttp request manager
  return ()

-- GET request
doGet :: FromJSON a => String -> Client a
doGet urlStr = do
  manager <- getManager
  url <- liftIO $  parseUrlThrow urlStr
  request <- applyAuth $ url {checkResponse = allowAny}
  responseLbs <- doHttp request manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throwC =<< (__f "Can't parse server response for GET request: {}" (Single err))
    Right res -> return res

-- OPTIONS request
doOptions :: FromJSON a => String -> Client a
doOptions urlStr = do
  manager <- getManager
  url <- liftIO $ parseUrlThrow urlStr
  let request = url {checkResponse = allowAny, method = "OPTIONS"}
  responseLbs <- doHttp request manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throwC =<< (__f "Can't parse server response for OPTIONS request: {}" (Single err))
    Right res -> return res

