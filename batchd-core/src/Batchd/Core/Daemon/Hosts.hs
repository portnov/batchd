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
    HostDriver (..),
    controllerFromConfig,
    -- * Local hosts driver
    localDriver
  ) where

import Control.Concurrent
import qualified Data.Map as M
import Data.Time
import Data.Aeson as Aeson
import System.Log.Heavy

import Batchd.Core.Common.Types

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
-- Host driver is a programmatic component (technically, an instance of
-- @HostDriver@ data type), which contains an implementation of
-- host-controlling functions.  I.e., this component knows how to start and
-- stop hosts of certain type.
--
-- Host controller is a configuration object, which refers to host driver which should
-- be actuall driving the hosts. Moreover, the host controller contains specific settings
-- used by the driver. There can exist many host controllers, using the same driver, but
-- with different settings.
--
-- Programmatically, batchd always deals with @HostController@, which is built from
-- configuration file by @HostDriver@.
--

-- | Host controller. Implementation of all functions is actually
-- located in host driver definition. These functions are already
-- aware of all settings in the host controller config file.
data HostController = HostController {
    -- | Name of host driver
    controllerDriverName :: String,

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
  show c = controllerDriverName c

-- | Host driver definition data type
data HostDriver = HostDriver {
    -- | Get name of the driver
    driverName :: String

    -- | Initialize host controllers from configuration file.
    -- This actually `plugs' knowledge of configuration into
    -- instance of @HostController@.
  , initController :: LoggingTState
                   -> Value -- ^ Configuration file contents
                   -> Either Error HostController
  }

-- | Utility function to construct host controller from configuration file.
controllerFromConfig :: FromJSON settings
               => String                                        -- ^ Driver name
               -> (settings -> LoggingTState -> HostController) -- ^ How to create controller from settings
               -> HostDriver
controllerFromConfig name maker =
    let init lts config =
          case fromJSON config of
            Aeson.Error err -> Left $ UnknownError err
            Aeson.Success settings -> Right $ maker settings lts
    in  HostDriver name init

-- | Local hosts driver
localDriver :: HostDriver
localDriver = HostDriver "local" $ \_ _ -> Right local

-- | Local hosts controller
local :: HostController
local = HostController {
    controllerDriverName = driverName localDriver,

    doesSupportStartStop = False,

    getActualHostName = \_ -> return Nothing,

    startHost = \_ -> return $ Right (),
    stopHost = \_ -> return $ Right ()
  }

