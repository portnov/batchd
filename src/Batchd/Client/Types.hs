{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module contains basic type definitions used by client.
module Batchd.Client.Types where

import GHC.Generics
import Control.Exception
import Data.Generics hiding (Generic)
import qualified Data.Text.Lazy as TL

-- | Client-side exception.
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

-- | User credentials.
type Credentials = (String, String)

