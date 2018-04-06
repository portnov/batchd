{-# LANGUAGE FlexibleInstances #-}
-- | This module contains definitions for logging in client.
-- Log messages in client are localized.
module Batchd.Client.Logging where

import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Client.Monad

putMessage :: (ClosedVarContainer vars) => Level -> Client TL.Text -> vars -> Client ()
putMessage v msg vars = do
  localizedMessage <- msg
  let msg = LogMessage v [] undefined localizedMessage vars []
  logMessage' msg

message :: ClosedVarContainer vars => Client TL.Text -> vars -> Client ()
message msg vars = putMessage info_level msg vars

verbose :: ClosedVarContainer vars => Client TL.Text -> vars -> Client ()
verbose msg vars = putMessage verbose_level msg vars

debug :: ClosedVarContainer vars => Client TL.Text -> vars -> Client ()
debug msg vars = putMessage debug_level msg vars

