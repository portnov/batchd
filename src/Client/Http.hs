{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Http where

import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Default
import Data.List
import Data.Aeson as Aeson
import Data.X509.CertificateStore
import Network.HTTP.Client
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status
import Network.HTTP.Client.TLS
import Network.TLS as TLS
import Network.TLS.Extra.Cipher
import Network.Connection

import Client.Types as C
import Client.Config
import Client.CmdLine (Batch (..))
import Common.Types

makeClientManager :: Batch -> IO Manager
makeClientManager opts = do
    cfg <- loadClientConfig
    url <- getManagerUrl (managerUrl opts) cfg
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

applyAuth :: C.Credentials -> Request -> Request
applyAuth (name,password) rq =
  applyBasicAuth (stringToBstr name) (stringToBstr password) rq
--   if not (secure rq) && host rq == "localhost"
--     then rq {requestHeaders = ("X-Auth-User", stringToBstr name) : requestHeaders rq}
--     else applyBasicAuth (stringToBstr name) (stringToBstr password) rq

handleStatus :: Response L.ByteString -> IO L.ByteString
handleStatus rs =
  if responseStatus rs == ok200
    then return $ responseBody rs
    else throw $ ClientException $ map (chr . fromIntegral) $ L.unpack $ responseBody rs

allowAny :: Request -> Response BodyReader -> IO ()
allowAny _ _ = return ()

doPut :: ToJSON a => Manager -> C.Credentials -> String -> a -> IO ()
doPut manager creds urlStr object = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url {
                  method="PUT",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  handleStatus =<< httpLbs request manager
  return ()

doPost :: ToJSON a => Manager -> C.Credentials -> String -> a -> IO ()
doPost manager creds urlStr object = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url {
                  method="POST",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  handleStatus =<< httpLbs request manager
  return ()

doDelete :: Manager -> C.Credentials -> String -> IO ()
doDelete manager creds urlStr = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url { method="DELETE",
                      checkResponse = allowAny
                    }
  handleStatus =<< httpLbs request manager
  return ()

doGet :: FromJSON a => Manager -> C.Credentials -> String -> IO a
doGet manager creds urlStr = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url {checkResponse = allowAny}
  -- print request
  responseLbs <- handleStatus =<< httpLbs request manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throw $ ClientException err
    Right res -> return res

