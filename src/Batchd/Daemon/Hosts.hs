{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Hosts where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Text.Format.Heavy
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
        r <- tryInitController selector lts name
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

ensureHostStarted :: LoggingTState -> Host -> IO ()
ensureHostStarted lts host = do
    c <- loadHostController lts (hController host)
    debugIO lts $(here) "Host `{}' is controlled by `{}'" (hName host, show c)
    start c (hName host)
  where
    start (AnyHostController controller) name = do
      if doesSupportStartStop controller
        then do
          debugIO lts $(here) "Starting host `{}'" (Single name)
          startHost controller (hControllerId host)
          debugIO lts $(here) "Waiting for host `{}' to initialize for {} seconds..." 
                              (name, hStartupTime host)
          threadDelay $ 1000 * 1000 * hStartupTime host
        else debugIO lts $(here) "Controller does not support starting hosts" ()

ensureHostStopped :: LoggingTState -> Host -> IO ()
ensureHostStopped lts host = do
    c <- loadHostController lts (hController host)
    debugIO lts $(here) "Host `{}' is controlled by `{}'" (hName host, show c)
    stop c (hName host)
  where
    stop (AnyHostController controller) name = do
      if doesSupportStartStop controller
        then do
          debugIO lts $(here) "Stopping host `{}'" (Single name)
          stopHost controller (hControllerId host)
        else debugIO lts $(here) "Controller does not support stopping hosts" ()

acquireHost :: LoggingTState -> HostCounters -> Host -> JobType -> IO ()
acquireHost lts mvar host jtype = do
    check (hName host)
    return ()
  where
    check name = do
      ok <- modifyMVar mvar $ \m -> do
              counter <- case M.lookup name m of
                           Just c -> return c
                           Nothing -> newMVar 0

              oldCount <- readMVar counter
              when (oldCount == 0) $ do
                ensureHostStarted lts host

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

releaseHost :: LoggingTState -> HostCounters -> Host -> IO ()
releaseHost lts mvar host = do
  let name = hName host
  modifyMVar_ mvar $ \m -> do
    counter <- case M.lookup name m of
                 Nothing -> fail $ "Can't release host: " ++ name ++ "; map: " ++ show (M.keys m)
                 Just c -> return c
    modifyMVar_ counter (\c -> return (c-1))
    newCount <- readMVar counter
    when (newCount == 0) $ do
      ensureHostStopped lts host
    return $ M.insert name counter m
  return ()

withHost :: HostCounters -> Host -> JobType -> Daemon a -> Daemon a
withHost mvar host jtype actions = do
  cfg <- askConfig
  pool <- askPool
  translations <- getTranslations
  lts <- askLoggingStateM
  let connInfo = ConnectionInfo cfg (Just pool) (Just translations)
  liftIO $ bracket_ (acquireHost lts mvar host jtype) (releaseHost lts mvar host) $ runDaemonIO connInfo lts actions

