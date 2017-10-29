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
import Data.Time
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Time () -- import instances only
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

waitForStatus :: MVar HostState -> [HostStatus] -> (HostState -> IO HostState) -> IO ()
waitForStatus mvar targetStatuses actions = do
  ok <- modifyMVar mvar $ \st ->
          if hsStatus st `elem` targetStatuses
            then do
              result <- actions st
              return (result, True)
            else do
              return (st, False)
  when (not ok) $ do
    threadDelay $ 10 * 1000 * 1000
    waitForStatus mvar targetStatuses actions

increaseJobCount :: LoggingTState -> HostState -> Maybe Int -> IO HostState
increaseJobCount lts st mbMaxJobs = do
    debugIO lts $(here) "Set status of host `{}' to {}" (hName $ hsHostConfig st, show activeOrBusy)
    return $ st {hsStatus = activeOrBusy, hsJobCount = hsJobCount st + 1}
  where
    activeOrBusy =
      case mbMaxJobs of
        Nothing -> Active
        Just maxJobs -> if hsJobCount st + 1 >= maxJobs 
                          then Busy
                          else Active

decreaseJobCount :: LoggingTState -> HostState -> IO HostState
decreaseJobCount lts st = do
    let newStatus = if hsJobCount st - 1 <= 0
                      then Released
                      else Active
    released <- if newStatus == Released
                  then Just <$> getCurrentTime
                  else return Nothing
    debugIO lts $(here) "Set status of host `{}' to {}" (hName $ hsHostConfig st, show newStatus)
    return $ st {hsStatus = newStatus, hsJobCount = hsJobCount st - 1, hsReleaseTime = released}

setHostStatus :: MVar HostState -> HostStatus -> IO ()
setHostStatus mvar status =
  modifyMVar_ mvar $ \st -> return $ st {hsStatus = status}

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
      counter <- modifyMVar mvar $ \m -> do
                   case M.lookup name m of
                     Just c -> do
                       return (m, c)
                     Nothing -> do
                       c <- newMVar $ HostState Free Nothing 0 host
                       let m' = M.insert name c m
                       return (m', c)

      waitForStatus counter [Free, Active, Released] $ \st -> do
        when (hsStatus st == Free) $ do
          ensureHostStarted lts host
        increaseJobCount lts st $ getMaxJobs host jtype

releaseHost :: LoggingTState -> HostCounters -> Host -> IO ()
releaseHost lts mvar host = do
  let name = hName host
  counter <- withMVar mvar $ \m -> do
               case M.lookup name m of
                 Nothing -> fail $ "Can't release host: " ++ name ++ "; map: " ++ show (M.keys m)
                 Just c -> return c
  modifyMVar_ counter $ \st -> do
    decreaseJobCount lts st
  return ()

withHost :: HostCounters -> Host -> JobType -> Daemon a -> Daemon a
withHost mvar host jtype actions = do
  cfg <- askConfig
  pool <- askPool
  translations <- getTranslations
  lts <- askLoggingStateM
  let connInfo = ConnectionInfo cfg (Just pool) (Just translations)
  liftIO $ bracket_ (acquireHost lts mvar host jtype) (releaseHost lts mvar host) $ runDaemonIO connInfo lts actions

hostCleaner :: LoggingTState -> HostCounters -> IO ()
hostCleaner lts mvar = forever $ do
  threadDelay $ 60 * 1000 * 1000
  hosts <- readMVar mvar
  forM_ (M.assocs hosts) $ \(name, counter) -> do
    modifyMVar_ counter $ \st -> do
      if hsStatus st == Released
        then case hsReleaseTime st of
               Nothing -> do
                 reportErrorIO lts $(here) "Host `{}' status is Released, but it's release time is not set"
                                           (Single name)
                 return st
               Just releaseTime -> do
                 now <- getCurrentTime
                 if ceiling (diffUTCTime now releaseTime) >= (5*60 :: Int)
                   then do
                        debugIO lts $(here) "Host `{}' was released at {}, it's time to shut it down"
                                            (name, releaseTime)
                        ensureHostStopped lts (hsHostConfig st)
                        return $ st {hsStatus = Free, hsReleaseTime = Nothing}
                   else do
                        debugIO lts $(here) "Host `{}' was relesed at {}, it's not time to shut it down yet"
                                            (name, releaseTime)
                        return st
        else return st
    
