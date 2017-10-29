{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Hosts where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map as M
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Localize
import Batchd.Core.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Daemon.Logging

#ifdef LIBVIRT
import Batchd.Ext.LibVirt
#endif
#ifdef DOCKER
import Batchd.Ext.Docker
#endif
#ifdef AWSEC2
import Batchd.Ext.AWS
#endif

supportedControllers :: [AnyHostControllerSelector]
supportedControllers =
  [
#ifdef LIBVIRT
   AnyHostControllerSelector LibVirtSelector,
#endif
#ifdef DOCKER
   AnyHostControllerSelector DockerSelector,
#endif
#ifdef AWSEC2
   AnyHostControllerSelector AWSEC2Selector,
#endif
   AnyHostControllerSelector LocalSelector
  ]

loadHostController :: LoggingTState -> FilePath -> IO AnyHostController
loadHostController lts name = do
    go (UnknownError "impossible: list of supported host controllers exhaused without errors") supportedControllers
  where
    go lastError [] = throw lastError
    go _ (AnyHostControllerSelector selector : rest) = do
        r <- tryInitController selector (ltsLogger lts) name
        case r of
          Right controller -> return $ AnyHostController controller
          Left err -> do
            debugIO lts $(here) "Loading host controller config by name `{0}': this is not a valid config for `{1}': {2}"
                   (name, controllerName selector, show err)
            go err rest

getMaxJobs :: Host -> JobType -> Maybe Int
getMaxJobs host jtype =
  case (hMaxJobs host, jtMaxJobs jtype) of
    (Nothing, Nothing) -> Nothing
    (Just m, Nothing) -> Just m
    (Nothing, Just m) -> Just m
    (Just x, Just y) -> Just $ max x y

acquireHost :: HostCounters -> Host -> JobType -> IO ()
acquireHost mvar host jtype = do
    check (hName host)
    return ()
  where
    check name = do
      ok <- modifyMVar mvar $ \m -> do
              counter <- case M.lookup name m of
                           Just c -> return c
                           Nothing -> newMVar 0
              let m' = M.insert name counter m
              case getMaxJobs host jtype of
                Just max -> do
                            ok <- modifyMVar counter $ \cnt -> do
                                    if cnt < max
                                      then return (cnt+1, True)
                                      else return (cnt, False)
                            return (m', ok)
                Nothing ->  do
                            modifyMVar_ counter (\c -> return (c+1))
                            return (m', True)
      if ok
        then return ()
        else do
             threadDelay $ 1000*1000
             check name

releaseHost :: HostCounters -> Host -> IO ()
releaseHost mvar host = do
  let name = hName host
  modifyMVar_ mvar $ \m -> do
    counter <- case M.lookup name m of
                 Nothing -> fail $ "Can't release host: " ++ name ++ "; map: " ++ show (M.keys m)
                 Just c -> return c
    modifyMVar_ counter (\c -> return (c-1))
    return $ M.insert name counter m
  return ()

withHost :: HostCounters -> Host -> JobType -> Daemon a -> Daemon a
withHost mvar host jtype actions = do
  cfg <- askConfig
  pool <- askPool
  translations <- getTranslations
  lts <- askLoggingStateM
  let connInfo = ConnectionInfo cfg (Just pool) (Just translations)
  liftIO $ bracket_ (acquireHost mvar host jtype) (releaseHost mvar host) $ runDaemonIO connInfo lts actions

