
module Client.Logging where

import Control.Monad (when)
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Parse

import Common.Localize
import Client.Types
import Client.Monad

putMessage :: (VarContainer vars) => Verbosity -> Client TL.Text -> vars -> Client ()
putMessage v msg vars = do
  localizedMessage <- msg
  verbosity <- gets csVerbosity
  when (v >= verbosity) $ do
      liftIO $ TLIO.putStrLn $ format (parseFormat' localizedMessage) vars

message :: VarContainer vars => Client TL.Text -> vars -> Client ()
message msg vars = putMessage Normal msg vars

verbose :: VarContainer vars => Client TL.Text -> vars -> Client ()
verbose msg vars = putMessage Verbose msg vars

debug :: VarContainer vars => Client TL.Text -> vars -> Client ()
debug msg vars = putMessage Debug msg vars

