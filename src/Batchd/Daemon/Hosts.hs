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
import qualified Data.HashMap.Strict as H
import Data.Time
import Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Time () -- import instances only
import System.Log.Heavy

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Core.Common.Localize
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types

#ifdef LIBVIRT
import Batchd.Ext.LibVirt
#endif
#ifdef DOCKER
import Batchd.Ext.Docker
#endif
#ifdef AWSEC2
import Batchd.Ext.AWS
#endif

supportedDrivers :: [HostDriver]
supportedDrivers =
  [
#ifdef LIBVIRT
   libVirtDriver,
#endif
#ifdef DOCKER
   dockerDriver,
#endif
#ifdef AWSEC2
   awsEc2Driver,
#endif
   localDriver
  ]

selectDriver :: String -> Maybe HostDriver
selectDriver name = go supportedDrivers
  where
    go [] = Nothing
    go (driver : rest)
      | driverName driver == name = Just driver
      | otherwise = go rest

loadHostController :: LoggingTState -> FilePath -> IO HostController
loadHostController lts name = do
    cfgr <- loadHostControllerConfig name
    case cfgr of
      Left err -> throw err
      Right value@(Object cfg) -> do
          case H.lookup "driver" cfg of
            Nothing -> throw $ UnknownError $ "Invalid host controller config: driver not specified"
            Just (Aeson.String dname) -> do
              case selectDriver (T.unpack dname) of
                Nothing -> throw $ UnknownError $ "Invalid host controller config: unsupported driver name: " ++ T.unpack dname
                Just driver -> do
                  case initController driver lts value of
                    Right controller -> return controller
                    Left err -> throw err
      Right _ -> throw $ UnknownError "Invalid host controller config: it must be a dictionary"

getMaxJobs :: Host -> JobType -> Maybe Int
getMaxJobs host jtype =
  case (hMaxJobs host, jtMaxJobs jtype) of
    (Nothing, Nothing) -> Nothing
    (Just m, Nothing) -> Just m
    (Nothing, Just m) -> Just m
    (Just x, Just y) -> Just $ max x y

waitForStatus :: LoggingTState -> MVar HostState -> [HostStatus] -> (HostState -> IO HostState) -> IO ()
waitForStatus lts mvar targetStatuses actions = do
  ok <- modifyMVar mvar $ \st ->
          if hsStatus st `elem` targetStatuses
            then do
              result <- actions st
              return (result, True)
            else do
              debugIO lts $(here) "Host `{}' has status {}, waiting for one of {}..."
                                  (hName $ hsHostConfig st, show (hsStatus st), show targetStatuses)
              return (st, False)
  when (not ok) $ do

    threadDelay $ 10 * 1000 * 1000
    waitForStatus lts mvar targetStatuses actions

increaseJobCount :: LoggingTState -> HostState -> Maybe Int -> IO HostState
increaseJobCount lts st mbMaxJobs = do
    debugIO lts $(here) "Host `{}' had {} jobs, got new one, set status to {}"
                        (hName $ hsHostConfig st, hsJobCount st, show activeOrBusy)
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
    debugIO lts $(here) "Host `{}' had {} jobs, one done, set status to {}"
                        (hName $ hsHostConfig st, hsJobCount st, show newStatus)
    return $ st {hsStatus = newStatus, hsJobCount = hsJobCount st - 1, hsReleaseTime = released}

setHostStatus :: MVar HostState -> HostStatus -> IO ()
setHostStatus mvar status =
  modifyMVar_ mvar $ \st -> return $ st {hsStatus = status}

ensureHostStarted :: LoggingTState -> Host -> IO ()
ensureHostStarted lts host = do
    controller <- loadHostController lts (hController host)
    debugIO lts $(here) "Host `{}' is controlled by `{}'" (hName host, show controller)
    let name = hName host
    if doesSupportStartStop controller
      then do
        debugIO lts $(here) "Starting host `{}'" (Single name)
        r <- startHost controller (hControllerId host)
        case r of
          Right _ -> do
              debugIO lts $(here) "Waiting for host `{}' to initialize for {} seconds..." 
                                  (name, hStartupTime host)
              threadDelay $ 1000 * 1000 * hStartupTime host
          Left err -> do
              throw err
      else debugIO lts $(here) "Controller does not support starting hosts" ()

ensureHostStopped :: LoggingTState -> Host -> IO ()
ensureHostStopped lts host = do
    controller <- loadHostController lts (hController host)
    debugIO lts $(here) "Host `{}' is controlled by `{}'" (hName host, show controller)
    let name = hName host
    if doesSupportStartStop controller
      then do
        debugIO lts $(here) "Stopping host `{}'" (Single name)
        r <- stopHost controller (hControllerId host)
        case r of
          Right _ -> return ()
          Left err -> throw err
      else debugIO lts $(here) "Controller does not support stopping hosts" ()

acquireHost :: LoggingTState -> HostsPool -> Host -> JobType -> IO ()
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

      waitForStatus lts counter [Free, Active, Released] $ \st -> do
        when (hsStatus st == Free) $ do
          ensureHostStarted lts host
        increaseJobCount lts st $ getMaxJobs host jtype

releaseHost :: LoggingTState -> HostsPool -> Host -> IO ()
releaseHost lts mvar host = do
  let name = hName host
  counter <- withMVar mvar $ \m -> do
               case M.lookup name m of
                 Nothing -> fail $ "Can't release host: " ++ name ++ "; map: " ++ show (M.keys m)
                 Just c -> return c
  modifyMVar_ counter $ \st -> do
    decreaseJobCount lts st
  return ()

withHost :: HostsPool -> Host -> JobType -> Daemon a -> Daemon (Either SomeException a)
withHost mvar host jtype actions = withLogVariable "host" (hName host) $ do
  cfg <- askConfig
  pool <- askPool
  translations <- getTranslations
  lts <- askLoggingStateM
  let connInfo = ConnectionInfo cfg (Just pool) (Just translations)
  liftIO $ try $ bracket_ (acquireHost lts mvar host jtype) (releaseHost lts mvar host) $ runDaemonIO connInfo lts actions

hostCleaner :: LoggingTState -> HostsPool -> IO ()
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
                 if ceiling (diffUTCTime now releaseTime) >= hShutdownTimeout (hsHostConfig st)
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
    
