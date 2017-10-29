{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

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

data HostStatus =
    Free            -- ^ Does not have any jobs, free for use
  | Starting        -- ^ During startup procedure. To give it a job, one has to wait for Free or Active status.
  | Active          -- ^ Has some jobs, but can accept one more.
  | Busy            -- ^ Has maximum allowed count of jobs. To give it a job, one has to wait for Active or Released status.
  | Released        -- ^ Has no jobs. Waiting to be stopped or for a new job.
  | Stopping        -- ^ During shutdown procedure.
  deriving (Eq, Show)

data HostState = HostState {
    hsStatus :: HostStatus
  , hsReleaseTime :: Maybe UTCTime -- ^ Time of last transition to Released state
  , hsJobCount :: Int
  , hsHostConfig :: Host
  }
  deriving (Show)

type HostName = String
type HostCounters = MVar (M.Map HostName (MVar HostState))

class Show c => HostController c where
  data Selector c

  controllerName :: Selector c -> String

  tryInitController :: Selector c -> LoggingTState -> FilePath -> IO (Either Error c)

  doesSupportStartStop :: c -> Bool

  startHost :: c -> HostName -> IO ()

  stopHost :: c -> HostName -> IO ()

data AnyHostController = forall c. HostController c => AnyHostController c

instance Show AnyHostController where
  show (AnyHostController c) = show c

data AnyHostControllerSelector = forall c. HostController c => AnyHostControllerSelector (Selector c)

instance Show AnyHostControllerSelector where
  show (AnyHostControllerSelector s) = controllerName s

data Local = Local

instance Show Local where
  show Local = "<local host controller>"

instance HostController Local where
  data Selector Local = LocalSelector
  doesSupportStartStop _ = False

  controllerName LocalSelector = "local"

  tryInitController _ _ "local" = return $ Right Local
  tryInitController _ _ name = return $ Left $ UnknownError "Invalid name for local host controller"

  startHost _ _ = return ()
  stopHost _ _ = return ()

