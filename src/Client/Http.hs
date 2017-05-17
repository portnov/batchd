{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Http where

import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Aeson as Aeson
import Network.HTTP.Client
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status

import Client.Types
import Common.Types

applyAuth :: Credentials -> Request -> Request
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

doPut :: ToJSON a => Manager -> Credentials -> String -> a -> IO ()
doPut manager creds urlStr object = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url {
                  method="PUT",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  handleStatus =<< httpLbs request manager
  return ()

doPost :: ToJSON a => Manager -> Credentials -> String -> a -> IO ()
doPost manager creds urlStr object = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url {
                  method="POST",
                  checkResponse = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  handleStatus =<< httpLbs request manager
  return ()

doDelete :: Manager -> Credentials -> String -> IO ()
doDelete manager creds urlStr = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url { method="DELETE",
                      checkResponse = allowAny
                    }
  handleStatus =<< httpLbs request manager
  return ()

doGet :: FromJSON a => Manager -> Credentials -> String -> IO a
doGet manager creds urlStr = do
  url <- parseUrl urlStr
  let request = applyAuth creds $ url {checkResponse = allowAny}
  -- print request
  responseLbs <- handleStatus =<< httpLbs request manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throw $ ClientException err
    Right res -> return res

