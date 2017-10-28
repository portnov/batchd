{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}

module Daemon.Hosts where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans
import qualified Data.Map as M

import Common.Types
import Common.Localize
import Daemon.Types

type HostName = String
type HostCounters = MVar (M.Map HostName (MVar Int))

class HostController c where
  doesSupportStartStop :: c -> Bool

  startHost :: c -> HostName -> IO ()

  stopHost :: c -> HostName -> IO ()

data AnyHostController = forall c. HostController c => AnyHostController c

data Local = Local
  deriving (Show)

instance HostController Local where
  doesSupportStartStop _ = False
  startHost _ _ = return ()
  stopHost _ _ = return ()

supportedControllers :: [(String, AnyHostController)]
supportedControllers = [("local", AnyHostController Local)]

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

