{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module System.Batchd.Daemon.Hosts where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map as M
import System.Log.Heavy

import System.Batchd.Common.Types
import System.Batchd.Common.Localize
import System.Batchd.Daemon.Types

type HostName = String
type HostCounters = MVar (M.Map HostName (MVar Int))

class HostController c where
  data Selector c

  initController :: Selector c -> SpecializedLogger -> IO c

  doesSupportStartStop :: c -> Bool

  startHost :: c -> HostName -> IO ()

  stopHost :: c -> HostName -> IO ()

data AnyHostController = forall c. HostController c => AnyHostController c

data AnyHostControllerSelector = forall c. HostController c => AnyHostControllerSelector (Selector c)

data Local = Local
  deriving (Show)

instance HostController Local where
  data Selector Local = LocalSelector
  doesSupportStartStop _ = False
  initController _ _ = return Local
  startHost _ _ = return ()
  stopHost _ _ = return ()

