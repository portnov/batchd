{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Http where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import Data.Char
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

import Common.Types
import Common.Config (getPassword)
import Client.Types as C
import Client.Config
import Client.CmdLine
import Client.Monad

makeClientManager :: CmdLine -> IO Manager
makeClientManager opts = do
    cfg <- loadClientConfig
    url <- getManagerUrl opts cfg
    if "https" `isPrefixOf` url
      then mkMngr (ccDisableServerCertificateCheck cfg) (ccCertificate cfg) (ccKey cfg) (ccCertificate cfg)
      else newManager defaultManagerSettings
  where
    mkMngr :: Bool -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> IO Manager
    mkMngr disableServerCertCheck mbCertFile mbKeyFile mbCaFile = do
     creds <- case (mbCertFile, mbKeyFile) of
                (Just certFile, Just keyFile) -> do
                    r <- credentialLoadX509 certFile keyFile
                    case r of
                      Right cr -> return $ Just cr
                      Left err -> fail $ "cant load certificate/key: " ++ show err
                _ -> do
                     putStrLn "certificate or key file is not specified, try to access HTTPS without them"
                     return Nothing
     mbStore <- case mbCaFile of
                       Nothing -> return Nothing
                       Just caFile -> do
                          r <- readCertificateStore caFile
                          case r of
                            Just store -> return $ Just store
                            Nothing -> fail $ "cannot read specified CA store"
     let shared = case mbStore of
                    Nothing -> def
                    Just store -> def { sharedCAStore = store }
                
     let clientCertHook _ = return creds
         skip _ _ _ _ = do
                putStrLn "server certificate check disabled"
                return []
         hooks = if disableServerCertCheck
                   then def {
                          onCertificateRequest = clientCertHook,
                          onServerCertificate = skip
                        }
                   else def {
                          onCertificateRequest = clientCertHook
                        }
         clientParams = (defaultParamsClient "" "")
                        { clientHooks = hooks,
                          clientUseServerNameIndication = True,
                          clientShared = shared,
                          clientSupported = def { supportedCiphers = ciphersuite_all, supportedVersions = [TLS12] }
                        }
         tlsSettings = TLSSettings clientParams

     newManager $ mkManagerSettings tlsSettings Nothing

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

obtainCredentials :: Client C.Credentials
obtainCredentials = do
  cfg <- gets csConfig
  opts@(CmdLine o _) <- gets csCmdline
  methods <- getAuthMethods
  let needPassword = BasicAuth `elem` methods
  name <- liftIO $ getUserName opts cfg
  pass <- if ccDisableAuth cfg || not needPassword
            then return ""
            else do
                 mbPassword <- liftIO $ getConfigParam' (password o) "BATCH_PASSWORD" (ccPassword cfg)
                 case mbPassword of
                   Just p -> return p
                   Nothing -> liftIO $ getPassword $ name ++ " password: "
  let creds = (name, pass)
  modify $ \st -> st {csCredentials = Just creds}
  return creds

getCredentials :: Client C.Credentials
getCredentials = do
  mbCreds <- gets csCredentials
  case mbCreds of
    Just creds -> return creds
    Nothing -> obtainCredentials

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

handleStatus :: Response L.ByteString -> IO L.ByteString
handleStatus rs =
  if responseStatus rs == ok200
    then return $ responseBody rs
    else throw $ ClientException $ map (chr . fromIntegral) $ L.unpack $ responseBody rs

allowAny :: Request -> Response BodyReader -> IO ()
allowAny _ _ = return ()

doHttp :: Request -> Manager -> Client L.ByteString
doHttp request manager = do
  -- liftIO $ print request
  liftIO $ handleStatus =<< httpLbs request manager

doPut :: ToJSON a => String -> a -> Client ()
doPut urlStr object = do
  manager <- gets csManager
  url <- liftIO $ parseUrl urlStr
  request <- applyAuth $ url {
                  method="PUT",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  doHttp request manager
  return ()

doPost :: ToJSON a => String -> a -> Client ()
doPost urlStr object = do
  manager <- gets csManager
  url <- liftIO $ parseUrl urlStr
  request <- applyAuth $ url {
                  method="POST",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  doHttp request manager
  return ()

doDelete :: String -> Client ()
doDelete urlStr = do
  manager <- gets csManager
  url <- liftIO $ parseUrl urlStr
  request <- applyAuth $ url { method="DELETE",
                      checkResponse = allowAny
                    }
  doHttp request manager
  return ()

doGet :: FromJSON a => String -> Client a
doGet urlStr = do
  manager <- gets csManager
  url <- liftIO $  parseUrl urlStr
  request <- applyAuth $ url {checkResponse = allowAny}
  -- print request
  responseLbs <- doHttp request manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throwC err
    Right res -> return res

doOptions :: FromJSON a => String -> Client a
doOptions urlStr = do
  manager <- gets csManager
  url <- liftIO $ parseUrl urlStr
  let request = url {checkResponse = allowAny, method = "OPTIONS"}
  responseLbs <- doHttp request manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throwC $ "Cant do OPTIONS: " ++ err
    Right res -> return res

