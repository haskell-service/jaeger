{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:      Control.Monad.Trans.JaegerMetrics.Prometheus
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDerivin , MultiParamTypeClasses, OverloadedStrings, RankNTypes, StandaloneDeriving, TypeFamilies, UndecidableInstances
--
-- A 'MonadJaegerMetrics' implementation for @Prometheus@ metrics.

module Control.Monad.Trans.JaegerMetrics.Prometheus (
    -- * 'PrometheusMetricsT' transformer
      PrometheusMetricsT
    , runPrometheusMetricsT
    -- ** The 'Metrics' used by 'PrometheusMetricsT'
    , Metrics
    , mkMetrics
    -- ** 'PrometheusMetricsT' over 'IO'
    , PrometheusMetrics
    , runPrometheusMetrics
    ) where

import Data.String (fromString)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader(ask, reader, local))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(liftBaseWith, restoreM), StM, ComposeSt,
    defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(liftWith, restoreT), StT, defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Writer.Class (MonadWriter)

import Control.Lens (Lens', (%=), _1, _2, _3, use, zoom)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap

import Prometheus (
    Counter, Gauge, Metric, Registry,
    addLabel, adjustGauge, counter, gauge, incCounter, incCounterBy,
    incGauge, register, setGauge)

import Control.Monad.Jaeger.Class (MonadJaeger)
import Control.Monad.JaegerMetrics.Class (
    MonadJaegerMetrics(addMetric, incMetric, resetMetric),
    metricId, metricHelp, metricLabels, metricName)
import qualified Control.Monad.JaegerMetrics.Class as M
import Control.Monad.JaegerTrace.Class (MonadJaegerTrace)

-- | The 'Metrics' structure contains all 'Counter's and 'Gauge's used by 'PrometheusMetricsT'.
data Metrics = Metrics { metricsTracesStartedSampled :: !Counter
                       , metricsTracesStartedNotSampled :: !Counter
                       , metricsTracesJoinedSampled :: !Counter
                       , metricsTracesJoinedNotSampled :: !Counter
                       , metricsSpansStarted :: !Counter
                       , metricsSpansFinished :: !Counter
                       , metricsSpansSampled :: !Counter
                       , metricsSpansNotSampled :: !Counter
                       , metricsReporterQueueLength :: !Gauge
                       , metricsReporterSuccess :: !Counter
                       , metricsReporterFailure :: !Counter
                       }

-- | Create 'Metrics' with 'Counter's and 'Gauge's registered in some 'Registry'.
mkMetrics :: (MonadState Registry m, MonadIO m) => m Metrics
mkMetrics = do
    registry <- get
    (metrics, (_, _, registry')) <- flip runStateT (mempty, mempty, registry) $
        Metrics <$> createCounter M.TracesStartedSampled
                <*> createCounter M.TracesStartedNotSampled
                <*> createCounter M.TracesJoinedSampled
                <*> createCounter M.TracesJoinedNotSampled
                <*> createCounter M.SpansStarted
                <*> createCounter M.SpansFinished
                <*> createCounter M.SpansSampled
                <*> createCounter M.SpansNotSampled
                <*> createGauge M.ReporterQueueLength
                <*> createCounter M.ReporterSuccess
                <*> createCounter M.ReporterFailure
    put registry'
    return metrics
  where
    -- This is an incredibly ugly and complicated hack, though required until
    -- https://github.com/ocharles/prometheus-effect/issues/5 is fixed.
    createCounter = create _1 counter
    createGauge = create _2 gauge
    create :: MonadIO m
           => Lens' (HashMap [Text] (Text -> IO Counter), HashMap [Text] (Text -> IO Gauge), Registry) (HashMap [Text] (Text -> IO a))
           -> Metric a
           -> M.Metric k
           -> StateT (HashMap [Text] (Text -> IO Counter), HashMap [Text] (Text -> IO Gauge), Registry) m a
    create l t m = let mid = metricId m in case metricLabels mid of
        (_:_:_) -> error "More than one label, unsupported"
        [] -> zoom _3 $ register (mkName $ metricName mid) (fromString $ Text.unpack $ metricHelp mid) mempty t
        [(labelName, labelValue)] -> do
            metrics <- use l
            case HashMap.lookup (metricName mid) metrics of
                Nothing -> do
                    fn <- zoom _3 $ register
                                        (mkName $ metricName mid)
                                        (fromString $ Text.unpack $ metricHelp mid)
                                        mempty
                                        (addLabel labelName t)
                    l %= HashMap.insert (metricName mid) fn
                    liftIO $ fn labelValue
                Just fn -> liftIO $ fn labelValue
    mkName = fromString . Text.unpack . Text.intercalate "_"

-- | Monad transformer to add 'MonadJaegerMetrics' functionality to a stack.
--
-- This implementation uses a 'Metrics' structure to keep 'Counter's and
-- 'Gauge's, and as such is meant to be used with @Prometheus@ reporting.
newtype PrometheusMetricsT m a = PrometheusMetricsT { unPrometheusMetricsT :: ReaderT Metrics m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger,
        MonadJaegerTrace)

deriving instance MonadResource m => MonadResource (PrometheusMetricsT m)

instance MonadReader r m => MonadReader r (PrometheusMetricsT m) where
    ask = lift ask
    reader = lift . reader
    local = mapPrometheusMetricsT . local

instance MonadRWS r w s m => MonadRWS r w s (PrometheusMetricsT m)

instance MonadTransControl PrometheusMetricsT where
    type StT PrometheusMetricsT a = StT (ReaderT Metrics) a
    liftWith = defaultLiftWith PrometheusMetricsT unPrometheusMetricsT
    restoreT = defaultRestoreT PrometheusMetricsT

instance MonadBaseControl b m => MonadBaseControl b (PrometheusMetricsT m) where
    type StM (PrometheusMetricsT m) a = ComposeSt PrometheusMetricsT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadBase IO m => MonadJaegerMetrics (PrometheusMetricsT m) where
    incMetric m = PrometheusMetricsT $ ask >>= \metrics -> liftBase $ case m of
        M.TracesStartedSampled -> incCounter (metricsTracesStartedSampled metrics)
        M.TracesStartedNotSampled -> incCounter (metricsTracesStartedNotSampled metrics)
        M.TracesJoinedSampled -> incCounter (metricsTracesJoinedSampled metrics)
        M.TracesJoinedNotSampled -> incCounter (metricsTracesJoinedNotSampled metrics)
        M.SpansStarted -> incCounter (metricsSpansStarted metrics)
        M.SpansFinished -> incCounter (metricsSpansFinished metrics)
        M.SpansSampled -> incCounter (metricsSpansSampled metrics)
        M.SpansNotSampled -> incCounter (metricsSpansNotSampled metrics)
        M.ReporterQueueLength -> incGauge (metricsReporterQueueLength metrics)
        M.ReporterSuccess -> incCounter (metricsReporterSuccess metrics)
        M.ReporterFailure -> incCounter (metricsReporterFailure metrics)

    addMetric m i = PrometheusMetricsT $ ask >>= \metrics -> liftBase $ case m of
        M.TracesStartedSampled -> incCounterBy (metricsTracesStartedSampled metrics) i'
        M.TracesStartedNotSampled -> incCounterBy (metricsTracesStartedNotSampled metrics) i'
        M.TracesJoinedSampled -> incCounterBy (metricsTracesJoinedSampled metrics) i'
        M.TracesJoinedNotSampled -> incCounterBy (metricsTracesJoinedNotSampled metrics) i'
        M.SpansStarted -> incCounterBy (metricsSpansStarted metrics) i'
        M.SpansFinished -> incCounterBy (metricsSpansFinished metrics) i'
        M.SpansSampled -> incCounterBy (metricsSpansSampled metrics) i'
        M.SpansNotSampled -> incCounterBy (metricsSpansNotSampled metrics) i'
        M.ReporterQueueLength -> adjustGauge (metricsReporterQueueLength metrics) (+ i')
        M.ReporterSuccess -> incCounterBy (metricsReporterSuccess metrics) i'
        M.ReporterFailure -> incCounterBy (metricsReporterFailure metrics) i'
      where
        i' :: Double
        i' = fromIntegral i

    resetMetric m = PrometheusMetricsT $ ask >>= \metrics -> liftBase $ case m of
        M.ReporterQueueLength -> setGauge (metricsReporterQueueLength metrics) 0

mapPrometheusMetricsT :: (m a -> n b) -> PrometheusMetricsT m a -> PrometheusMetricsT n b
mapPrometheusMetricsT f = PrometheusMetricsT . mapReaderT f . unPrometheusMetricsT

-- | Execute a 'PrometheusMetricsT' action, collecting metrics in the given 'Metrics'.
runPrometheusMetricsT :: PrometheusMetricsT m a -> Metrics -> m a
runPrometheusMetricsT = runReaderT . unPrometheusMetricsT


-- | 'PrometheusMetricsT' applied over 'IO', for ease-of-use.
type PrometheusMetrics = PrometheusMetricsT IO

-- | 'runPrometheusMetricsT' for 'PrometheusMetrics'.
runPrometheusMetrics :: PrometheusMetrics a -> Metrics -> IO a
runPrometheusMetrics = runPrometheusMetricsT
