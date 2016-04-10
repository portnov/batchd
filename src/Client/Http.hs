{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Http where

import Control.Exception
import qualified Data.ByteString.Lazy as L
import Data.Aeson as Aeson
import Network.HTTP.Client
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status

import Common.Config
import Client.Types
import Client.CmdLine

handleStatus :: Response L.ByteString -> IO L.ByteString
handleStatus rs =
  if responseStatus rs == ok200
    then return $ responseBody rs
    else throw $ ClientException $ show $ responseBody rs

allowAny :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
allowAny _ _ _ = Nothing

doPut :: ToJSON a => Manager -> String -> a -> IO ()
doPut manager urlStr object = do
  url <- parseUrl urlStr
  let request = url {
                  method="PUT",
                  checkStatus = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  handleStatus =<< httpLbs request manager
  return ()

doPost :: ToJSON a => Manager -> String -> a -> IO ()
doPost manager urlStr object = do
  url <- parseUrl urlStr
  let request = url {
                  method="POST",
                  checkStatus = allowAny,
                  requestBody = RequestBodyLBS $ Aeson.encode object
                }
  handleStatus =<< httpLbs request manager
  return ()

doDelete :: Manager -> String -> IO ()
doDelete manager urlStr = do
  url <- parseUrl urlStr
  let request = url { method="DELETE",
                      checkStatus = allowAny
                    }
  handleStatus =<< httpLbs request manager
  return ()

doGet :: FromJSON a => Manager -> String -> IO a
doGet manager urlStr = do
  url <- parseUrl urlStr
  let reqest = url {checkStatus = allowAny}
  responseLbs <- handleStatus =<< httpLbs url manager
  case Aeson.eitherDecode responseLbs of
    Left err -> throw $ ClientException err
    Right res -> return res

