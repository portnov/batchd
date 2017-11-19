{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Batchd.Daemon.Monitoring 
  (
    WaiMetrics,
    setupMetrics,
    metricsDumper, metricsCleaner,
    getWaiMetricsMiddleware,
    getCurrentMetrics,
    metricRecordToJsonTree,
    metricRecordToJsonPlain,
    sampleToJsonPlain,
    Metrics.counter,
    Metrics.gauge,
    Metrics.timed,
    timedN,
    Metrics.distribution,
    Metrics.label
  ) where

import Control.Monad
import Control.Monad.State
import Control.Concurrent
import qualified Control.Monad.Metrics as Metrics
import Control.Monad.Catch as MC
import Lens.Micro
import Data.Time
import qualified Data.Vector as V
import Data.Aeson as Aeson
import Data.Aeson.Types as Aeson
import qualified Data.Text as T
import Data.Text.Format.Heavy
import qualified Data.HashMap.Lazy as H
import qualified Data.HashMap.Strict as M
import Database.Persist
import qualified System.Metrics as EKG
import qualified System.Metrics.Distribution as EKG
import qualified Network.Wai as Wai
import Network.Wai.Metrics

import Batchd.Core.Daemon.Logging
import Batchd.Common.Types
import Batchd.Common.Data
import Batchd.Daemon.Types

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

getCurrentMetrics :: Maybe T.Text -> Daemon EKG.Sample
getCurrentMetrics mbPrefix = do
    metrics <- Metrics.getMetrics
    let store = metrics ^. Metrics.metricsStore
    sample <- liftIO $ EKG.sampleAll store
    let good = case mbPrefix of
                 Just prefix -> H.filterWithKey (\name _ -> prefix `T.isPrefixOf` name) sample
                 Nothing -> sample
    return good

metricsDumper :: Daemon ()
metricsDumper = do
  cfg <- askConfig
  let mode = dbcDaemonMode cfg
  let timeout = mcDumpTimeout $ dbcMetrics cfg
  forever $ do
    liftIO $ threadDelay $ timeout * 1000 * 1000
    now <- liftIO $ getCurrentTime
    sample <- getCurrentMetrics Nothing
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
      EKG.Counter n  -> (emptyRecord Counter) {metricRecordValue = Just n}
      EKG.Gauge n    -> (emptyRecord Gauge)   {metricRecordValue = Just n}
      EKG.Label text -> (emptyRecord Label)   {metricRecordText = Just text}
      EKG.Distribution st ->
        (emptyRecord Distribution) {
          metricRecordMean = Just $ EKG.mean st,
          metricRecordVariance = Just $ EKG.variance st,
          metricRecordCount = Just $ EKG.count st,
          metricRecordSum = Just $ EKG.sum st,
          metricRecordMin = Just $ EKG.min st,
          metricRecordMax = Just $ EKG.max st
        }
  where
    emptyRecord kind = MetricRecord name time mode kind Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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

-- metricValueToJson :: MetricRecord -> Aeson.Value
-- metricValueToJson r =
--   case metricRecordKind r of
--     Counter -> object ["type" .= ("c" :: T.Text), "val" .= metricRecordValue r]
--     Gauge   -> object ["type" .= ("g" :: T.Text), "val" .= metricRecordValue r]
--     Label   -> object ["type" .= ("l" :: T.Text), "val" .= metricRecordText r]
--     Distribution ->
--       object [
--         "type" .= ("d" :: T.Text), 
--         "mean" .= metricRecordMean r,
--         "variance" .= metricRecordVariance r,
--         "count" .= metricRecordCount r,
--         "sum" .= metricRecordSum r,
--         "min" .= metricRecordMin r,
--         "max" .= metricRecordMax r
--       ]

metricRecordToJsonPlain :: MetricRecord -> Aeson.Value
metricRecordToJsonPlain r = toJSON r

sampleToJsonPlain :: UTCTime -> EKG.Sample -> Aeson.Value
sampleToJsonPlain time sample = Aeson.Array $ V.fromList $ map convert $ H.toList sample
  where
    convert (name, value) = toJSON $ mkRecord Manager time name value

metricRecordToJsonTree :: MetricRecord -> Aeson.Value
metricRecordToJsonTree r =
  let metricValue = toJSON r

      build :: Aeson.Value -> T.Text -> Aeson.Value -> Aeson.Value
      build m name val = go m (T.splitOn "." name) val

      go :: Aeson.Value -> [T.Text] -> Aeson.Value -> Aeson.Value
      go (Aeson.Object m) [str] val      = Aeson.Object $ M.insert str val m
      go (Aeson.Object m) (str:rest) val = case M.lookup str m of
          Nothing -> Aeson.Object $ M.insert str (go Aeson.emptyObject rest val) m
          Just m' -> Aeson.Object $ M.insert str (go m' rest val) m
      go v _ _                        = error $ "metricToRecordJson.go: unexpected: " ++ show v

  in  build Aeson.emptyObject (metricRecordName r) metricValue

timedN :: (MonadIO m, Metrics.MonadMetrics m, MonadMask m) => [T.Text] -> m a -> m a
timedN = Metrics.timedList Metrics.Seconds

