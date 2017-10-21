{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Client.Types where

import GHC.Generics
import Control.Exception
import Data.Generics hiding (Generic)
import qualified Data.Text.Lazy as TL

data ClientException = ClientException TL.Text
  deriving (Data, Typeable, Generic)

instance Exception ClientException

instance Show ClientException where
  show (ClientException e) = TL.unpack e

data CrudMode =
    View
  | Add
  | Update
  | Delete
  deriving (Eq, Show, Data, Typeable)

type Credentials = (String, String)

