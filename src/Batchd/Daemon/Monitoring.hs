{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Monitoring 
  (
    WaiMetrics,
    setupMetrics,
    metricsDumper, metricsCleaner,
    getWaiMetricsMiddleware,
    Metrics.counter,
    Metrics.gauge,
    Metrics.timed,
    Metrics.distribution,
    Metrics.label
  ) where

import Control.Monad
import Control.Monad.State
import Control.Concurrent
import qualified Control.Monad.Metrics as Metrics
import Lens.Micro
import Data.Time
import qualified Data.Text as T
import Data.Text.Format.Heavy
import qualified Data.HashMap.Lazy as H
import Database.Persist
import qualified System.Metrics as EKG
import qualified System.Metrics.Distribution as EKG
import qualified Network.Wai as Wai
import Network.Wai.Metrics

import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Daemon.Types
-- import Batchd.Daemon.

setupMetrics :: Daemon ()
setupMetrics = do
  store <- liftIO EKG.newStore
  cfg <- askConfig
  when (mcGcMetrics $ dbcMetrics cfg) $ do
      liftIO $ EKG.registerGcMetrics store
  mbWaiMetrics <- if mcHttpMetrics $ dbcMetrics cfg
                    then Just `fmap` (liftIO $ registerNamedWaiMetrics "batchd" store)
                    else return Nothing
  metrics <- liftIO $ Metrics.initializeWith store
  Daemon $ lift $ modify $ \st -> st {ciMetrics = Just metrics, ciWaiMetrics = mbWaiMetrics}

instance Metrics.MonadMetrics Daemon where
  getMetrics = do
    ci <- askConnectionInfo
    case ciMetrics ci of
      Nothing -> fail $ "Metrics are not initialized yet!"
      Just m -> return m

getWaiMetricsMiddleware :: Daemon (Maybe Wai.Middleware)
getWaiMetricsMiddleware = do
  ci <- askConnectionInfo
  case ciWaiMetrics ci of
    Nothing -> return Nothing
    Just wm -> return $ Just $ metrics wm

metricsDumper :: Daemon ()
metricsDumper = do
  cfg <- askConfig
  let mode = dbcDaemonMode cfg
  let timeout = mcDumpTimeout $ dbcMetrics cfg
  forever $ do
    liftIO $ threadDelay $ timeout * 1000 * 1000
    metrics <- Metrics.getMetrics
    let store = metrics ^. Metrics.metricsStore
    sample <- liftIO $ EKG.sampleAll store
    now <- liftIO $ getCurrentTime
    r <- runDB $ do
            forM_ (H.toList sample) $ \(name, value) -> do
              let ok = case mcStorePrefixOnly $ dbcMetrics cfg of
                         Nothing -> True
                         Just prefix -> prefix `T.isPrefixOf` name
              when ok $ do
                  let record = mkRecord mode now name value
                  insert_ record
    case r of
      Left err -> $reportError "Can't insert metric record to DB: {}" (Single $ show err)
      Right _ -> return ()

mkRecord :: DaemonMode -> UTCTime -> T.Text -> EKG.Value -> MetricRecord
mkRecord mode time name value =
    case value of
      EKG.Counter n -> emptyRecord {metricRecordValue = Just n}
      EKG.Gauge n   -> emptyRecord {metricRecordValue = Just n}
      EKG.Label text -> emptyRecord {metricRecordText = Just text}
      EKG.Distribution st ->
        emptyRecord {
          metricRecordMean = Just $ EKG.mean st,
          metricRecordVariance = Just $ EKG.variance st,
          metricRecordCount = Just $ EKG.count st,
          metricRecordSum = Just $ EKG.sum st,
          metricRecordMin = Just $ EKG.min st,
          metricRecordMax = Just $ EKG.max st
        }
  where
    emptyRecord = MetricRecord mode name time Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

metricsCleaner :: Daemon ()
metricsCleaner = do
  cfg <- askConfig
  let days = scMetricRecords $ dbcStorage cfg
  forever $ do
    liftIO $ threadDelay $ 60 * 60 * 1000*1000
    now <- liftIO $ getCurrentTime
    let delta = fromIntegral $ days * 24 * 3600
    let edge = addUTCTime (negate delta) now
    r <- runDB $ do
           deleteWhere [MetricRecordTime <. edge]
    case r of
      Left err -> $reportError "Can't clean metrics history: {}" (Single $ show err)
      Right _ -> return ()

