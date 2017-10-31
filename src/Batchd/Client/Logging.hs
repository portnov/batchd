{-# LANGUAGE FlexibleInstances #-}
module Batchd.Client.Logging where

import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Client.Monad

putMessage :: (VarContainer vars) => Level -> Client TL.Text -> vars -> Client ()
putMessage v msg vars = do
  localizedMessage <- msg
  let msg = LogMessage v [] undefined localizedMessage vars []
  logMessage' msg

message :: VarContainer vars => Client TL.Text -> vars -> Client ()
message msg vars = putMessage info_level msg vars

verbose :: VarContainer vars => Client TL.Text -> vars -> Client ()
verbose msg vars = putMessage verbose_level msg vars

debug :: VarContainer vars => Client TL.Text -> vars -> Client ()
debug msg vars = putMessage debug_level msg vars

