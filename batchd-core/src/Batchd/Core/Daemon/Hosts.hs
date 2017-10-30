{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module contains generic definitions about remote hosts.
module Batchd.Core.Daemon.Hosts where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Time
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import Batchd.Core.Daemon.Types

-- | Host status
data HostStatus =
    Free            -- ^ Does not have any jobs, free for use
  | Active          -- ^ Has some jobs, but can accept one more.
  | Busy            -- ^ Has maximum allowed count of jobs. To give it a job, one has to wait for Active or Released status.
  | Released        -- ^ Has no jobs. Waiting to be stopped or for a new job.
  deriving (Eq, Show)

-- | Host state
data HostState = HostState {
    hsStatus :: HostStatus
  , hsReleaseTime :: Maybe UTCTime -- ^ Time of last transition to Released state
  , hsJobCount :: Int              -- ^ Count of jobs currently executed by the host
  , hsHostConfig :: Host
  }
  deriving (Show)

-- | Host name
type HostName = String

-- | Pool of used hosts and their states.
type HostCounters = MVar (M.Map HostName (MVar HostState))

-- | Type class of host controllers.
class Show c => HostController c where
  -- | Proxy data type, which is used to select controller implementation.
  -- Usually should have one trivial constructor.
  data Selector c

  -- | Get name of the controller
  controllerName :: Selector c -> String

  -- | Try initialize controller from specified config file.
  -- Should return Left if config file is not valid for this controller.
  tryInitController :: Selector c
                    -> LoggingTState
                    -> FilePath            -- ^ Name of config file without extension (@"docker"@)
                    -> IO (Either Error c)

  -- | Does this controller support starting and stopping hosts?
  doesSupportStartStop :: c -> Bool

  -- | Try to obtain actual network hostname for the host.
  -- May be useful if VMs can change their IPs at each startup.
  -- Default implementation always returns Nothing.
  getActualHostName :: c -> HostName -> IO (Maybe HostName)
  getActualHostName _ _ = return Nothing

  -- | Start the host. Should not return error if the 
  -- host is already started.
  startHost :: c -> HostName -> IO (Either Error ())

  -- | Shutdown the host. Should wait until the host is
  -- actually shut down.
  stopHost :: c -> HostName -> IO (Either Error ())

-- | Container data type for any host controller
data AnyHostController = forall c. HostController c => AnyHostController c

instance Show AnyHostController where
  show (AnyHostController c) = show c

-- | Container data type for any host controller selector
data AnyHostControllerSelector = forall c. HostController c => AnyHostControllerSelector (Selector c)

instance Show AnyHostControllerSelector where
  show (AnyHostControllerSelector s) = controllerName s

-- | Local hosts controller
data Local = Local

instance Show Local where
  show Local = "<local host controller>"

instance HostController Local where
  data Selector Local = LocalSelector
  doesSupportStartStop _ = False

  controllerName LocalSelector = "local"

  tryInitController _ _ "local" = return $ Right Local
  tryInitController _ _ name = return $ Left $ UnknownError "Invalid name for local host controller"

  startHost _ _ = return $ Right ()
  stopHost _ _ = return $ Right ()

