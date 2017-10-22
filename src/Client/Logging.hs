{-# LANGUAGE FlexibleInstances #-}
module Client.Logging where

import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Logger (LogLevel (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse
import System.Log.Heavy

import Common.Localize
import Client.Types
import Client.CmdLine
import Client.Monad

putMessage :: (VarContainer vars) => LogLevel -> Client TL.Text -> vars -> Client ()
putMessage v msg vars = do
  localizedMessage <- msg
  let msg = LogMessage v [] undefined localizedMessage vars []
  logMessage' msg

message :: VarContainer vars => Client TL.Text -> vars -> Client ()
message msg vars = putMessage LevelInfo msg vars

verbose :: VarContainer vars => Client TL.Text -> vars -> Client ()
verbose msg vars = putMessage LevelInfo msg vars

debug :: VarContainer vars => Client TL.Text -> vars -> Client ()
debug msg vars = putMessage LevelDebug msg vars

