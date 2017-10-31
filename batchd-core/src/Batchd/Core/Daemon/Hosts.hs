{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module contains generic definitions about remote hosts.
module Batchd.Core.Daemon.Hosts
  ( -- * Host-related data types
    HostStatus (..),
    HostState (..),
    HostName,
    HostsPool,
    -- * Host controllers
    -- $drivers
    HostController (..),
    HostDriver,
    controllerFromConfig,
    -- * Local hosts driver
    initLocal
  ) where

import Control.Concurrent
import qualified Data.Map as M
import Data.Time
import Data.Aeson
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config

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
type HostsPool = MVar (M.Map HostName (MVar HostState))

-- $drivers
--
-- Host driver is a programmatic component (technically, a function which
-- constructs an instance of @HostController@ data type from controller
-- settings), which contains an implementation of host-controlling functions.
-- I.e., this component knows how to start and stop hosts of certain type.
--
-- Host controller is a configuration object, which refers to host driver which should
-- be actuall driving the hosts. Moreover, the host controller contains specific settings
-- used by the driver. There can exist many host controllers, using the same driver, but
-- with different settings.
--

-- | ADT for host host controlling drivers.
data HostController = HostController {
    -- | Get name of the driver
    driverName :: String,

    -- | Does this controller support starting and stopping hosts?
    doesSupportStartStop :: Bool,

    -- | Try to obtain actual network hostname for the host.
    -- May be useful if VMs can change their IPs at each startup.
    getActualHostName :: HostName -> IO (Maybe HostName),

    -- | Start the host. Should not return error if the 
    -- host is already started.
    startHost :: HostName -> IO (Either Error ()),

    -- | Shutdown the host. Should wait until the host is
    -- actually shut down.
    stopHost :: HostName -> IO (Either Error ())
  }

instance Show HostController where
  show c = driverName c

-- | A type of functions which initialize host controllers from configuration file.
-- The second argument is name of config file without extension (e.g., @"docker"@).
type HostDriver = LoggingTState -> FilePath -> IO (Either Error HostController)

-- | Utility function to construct host controller from configuration file.
controllerFromConfig :: FromJSON settings
               => (settings -> LoggingTState -> HostController)
               -> HostDriver
controllerFromConfig maker lts name = do
    r <- loadHostControllerConfig name
    case r of
      Left err -> return $ Left err
      Right settings -> return $ Right $ maker settings lts

-- | Initialize local hosts driver
initLocal :: HostDriver
initLocal _ "local" = return $ Right local
initLocal _ _ = return $ Left $ UnknownError "Invalid name for local host controller"

-- | Local hosts driver
local :: HostController
local = HostController {
    doesSupportStartStop = False,

    getActualHostName = \_ -> return Nothing,

    driverName = "local",

    startHost = \_ -> return $ Right (),
    stopHost = \_ -> return $ Right ()
  }

