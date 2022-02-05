
module Batchd.Daemon.Crypto where

import Crypto.Hash
import Crypto.Random

import Batchd.Core.Common.Types

-- for generating the salt
randomString :: Int -> IO String
randomString size = do
  drg <- getSystemDRG
  let (bytes, _) = randomBytesGenerate size drg
  return $ bstrToString bytes

randomSalt :: IO String
randomSalt = randomString 16

calcHash :: String -> String -> String -> String
calcHash password dynamicSalt staticSalt =
  let str = password <> dynamicSalt <> staticSalt
      bstr = stringToBstr str
  in  show (hash bstr :: Digest SHA3_256)

