{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Batchd.Daemon.Hosts where

import Control.Monad
import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Control.Monad.Catch as MC
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Data.Time
import Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Format.Heavy
import Data.Text.Format.Heavy.Time () -- import instances only
import Data.Text.Format.Heavy.Parse.Shell
import System.Log.Heavy
import System.Exit (ExitCode (..))

import Batchd.Core.Common.Types
import Batchd.Core.Common.Config
import Batchd.Daemon.Types
import Batchd.Core.Daemon.Hosts
import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Daemon.Monitoring as Monitoring
import Batchd.Daemon.SSH (execCommandsOnHost)
import Batchd.Daemon.Local (execLocalCommands)

#ifdef LIBVIRT
import Batchd.Ext.LibVirt
#endif
#ifdef DOCKER
import Batchd.Ext.Docker
#endif
#ifdef AWSEC2
import Batchd.Ext.AWS
#endif
#ifdef LINODE
import Batchd.Ext.Linode
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
#ifdef LINODE
    linodeDriver,
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

loadHostController :: LoggingTState -> T.Text -> IO HostController
loadHostController lts "local" = do
  case initController localDriver lts undefined of
    Right controller -> return controller
    Left err -> throw $ UnknownError $ "Can't initialize local host controller: " ++ show err
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

waitForStatus :: MVar HostState -> [HostStatus] -> (HostState -> Daemon HostState) -> Daemon ()
waitForStatus mvar targetStatuses actions = do
  connInfo <- askConnectionInfo
  lts <- askLoggingStateM
  ok <- liftIO $ modifyMVar mvar $ \st ->
          if hsStatus st `elem` targetStatuses
            then do
              result <- runDaemonIO connInfo lts $ actions st
              return (result, True)
            else do
              debugIO lts $(here) "Host `{}' has status {}, waiting for one of {}..."
                                  (hName $ hsHostConfig st, show (hsStatus st), show targetStatuses)
              return (st, False)

  when (not ok) $ do
    liftIO $ threadDelay $ 10 * 1000 * 1000
    waitForStatus mvar targetStatuses actions

increaseJobCount :: HostState -> Maybe Int -> Daemon HostState
increaseJobCount st mbMaxJobs = do
    $debug "Host `{}' had {} jobs, got new one, set status to {}"
                        (hName $ hsHostConfig st, hsJobCount st, show activeOrBusy)
    return $ st {hsStatus = activeOrBusy, hsJobCount = hsJobCount st + 1}
  where
    activeOrBusy =
      case mbMaxJobs of
        Nothing -> Active
        Just maxJobs -> if hsJobCount st + 1 >= maxJobs 
                          then Busy
                          else Active

decreaseJobCount :: HostState -> Daemon HostState
decreaseJobCount st = do
    let newStatus = if hsJobCount st - 1 <= 0
                      then Released
                      else Active
    released <- if newStatus == Released
                  then Just <$> liftIO getCurrentTime
                  else return Nothing
    $debug "Host `{}' had {} jobs, one done, set status to {}"
                        (hName $ hsHostConfig st, hsJobCount st, show newStatus)
    return $ st {hsStatus = newStatus, hsJobCount = hsJobCount st - 1, hsReleaseTime = released}

setHostStatus :: MVar HostState -> HostStatus -> IO ()
setHostStatus mvar status =
  modifyMVar_ mvar $ \st -> return $ st {hsStatus = status}

formatHostCommand :: GlobalConfig -> Host -> T.Text -> T.Text
formatHostCommand cfg host template =
    TL.toStrict $ format (parseShellFormat' $ TL.fromStrict template) context
  where
    context = optional $ hVariables host `ThenCheck` hostParams `ThenCheck` dbcVariables cfg
    hostParams :: Variables
    hostParams = M.fromList $ [
                    ("name", hName host),
                    ("hostname", hHostName host),
                    ("controllerId", hControllerId host),
                    -- ("publicKey", hPublicKey host),
                    -- ("privateKey", hPrivateKey host),
                    ("passphrase", hPassphrase host),
                    ("user", hUserName host),
                    ("port", T.pack $ show $ hPort host),
                    ("inputDirectory", T.pack $ hInputDirectory host),
                    ("outputDirectory", T.pack $ hOutputDirectory host)
                  ]

ensureHostStarted :: Host -> Daemon ()
ensureHostStarted host = do
    lts <- askLoggingStateM
    cfg <- askConfig
    controller <- liftIO $ loadHostController lts (hController host)
    $debug "Host `{}' is controlled by `{}'" (hName host, show controller)
    let name = hName host
    if doesSupportStartStop controller
      then do
        $debug "Starting host `{}'" (Single name)
        r <- liftIO $ startHost controller (hControllerId host)
        case r of
          Right _ -> do
              $debug "Waiting for host `{}' to initialize for {} seconds..." 
                                  (name, hStartupTime host)
              liftIO $ threadDelay $ 1000 * 1000 * hStartupTime host
              let localCommands = map (formatHostCommand cfg host) (hStartupDispatcherCommands host)
              ec <- liftIO $ execLocalCommands lts localCommands
              case ec of
                ExitFailure rc -> throw $ HostInitializationError rc
                ExitSuccess -> do
                  let commandsOnHost = map (formatHostCommand cfg host) (hStartupHostCommands host)
                  ec <- execCommandsOnHost controller host commandsOnHost
                  case ec of
                    ExitSuccess -> return ()
                    ExitFailure rc -> throw $ HostInitializationError rc
          Left err -> throw err
      else $debug "Controller does not support starting hosts" ()

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

acquireHost :: HostsPool -> Host -> JobType -> Daemon ()
acquireHost mvar host jtype = do
    check (hName host)
    return ()
  where
    check :: T.Text -> Daemon()
    check name = do
      counter <- liftIO $ modifyMVar mvar $ \m -> do
                   case M.lookup name m of
                     Just c -> do
                       return (m, c)
                     Nothing -> do
                       c <- newMVar $ HostState Free Nothing 0 host
                       let m' = M.insert name c m
                       return (m', c)

      waitForStatus counter [Free, Active, Released] $ \st -> do
        when (hsStatus st == Free) $ do
          ensureHostStarted host
        increaseJobCount st $ getMaxJobs host jtype

releaseHost :: HostsPool -> Host -> Daemon ()
releaseHost mvar host = do
  let name = hName host
  counter <- liftIO $ withMVar mvar $ \m -> do
               case M.lookup name m of
                 Nothing -> fail $ "Can't release host: " <> T.unpack name <> "; map: " <> show (M.keys m)
                 Just c -> return c
  wrapDaemon_ (modifyMVar_ counter) $ \st -> decreaseJobCount st
  return ()

withHost :: HostsPool -> Host -> JobType -> Daemon a -> Daemon (Either SomeException a)
withHost mvar host jtype actions = withLogVariable "host" (hName host) $ do
  MC.try $ MC.bracket_ (acquireHost mvar host jtype) (releaseHost mvar host) actions

hostsMetricDumper :: HostsPool -> Daemon ()
hostsMetricDumper pool = forever $ do
  liftIO $ threadDelay $ 10 * 1000 * 1000
  hosts <- liftIO $ readMVar pool
  statuses <- forM (M.assocs hosts) $ \(name, stVar) -> do
                st <- liftIO $ readMVar stVar
                Monitoring.gauge ("batchd.host." <> name <> ".jobs") $ hsJobCount st
                Monitoring.label ("batchd.host." <> name <> ".status") $ T.pack $ show $ hsStatus st
                return $ hsStatus st
  let active = length $ filter (== Active) statuses
  let busy = length $ filter (== Busy) statuses
  Monitoring.gauge "batchd.hosts.active" active
  Monitoring.gauge "batchd.hosts.busy" busy

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
    
