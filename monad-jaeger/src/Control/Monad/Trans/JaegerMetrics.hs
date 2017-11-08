{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:      Control.Monad.Trans.JaegerMetrics
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, StandaloneDeriving, TypeFamilies, UndecidableInstances
--
-- Instances of 'MonadJaegerMetrics'.

module Control.Monad.Trans.JaegerMetrics (
    -- * 'JaegerMetricsT' transformer
      JaegerMetricsT
    , runJaegerMetricsT
    -- ** The 'Metrics' used by 'JaegerMetricsT'
    , Metrics
    , mkMetrics
    -- ** 'JaegerMetricsT' over 'IO'
    , JaegerMetrics
    , runJaegerMetrics
    -- * 'NoJaegerMetricsT' transformer
    , NoJaegerMetricsT
    , runNoJaegerMetricsT
    -- ** 'NoJaegerMetricsT' over 'IO'
    , NoJaegerMetrics
    , runNoJaegerMetrics
    ) where

import Data.Int (Int64)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader(ask, reader, local))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(liftBaseWith, restoreM), StM, ComposeSt,
    defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(liftWith, restoreT), StT, defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Reader (ReaderT, mapReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)

import qualified Data.Text as Text

import System.Metrics (Store, createCounter, createGauge)
import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as Counter
import System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge

import Control.Monad.Jaeger.Class (MonadJaeger)
import Control.Monad.JaegerMetrics.Class (
    MonadJaegerMetrics(addMetric, incMetric, resetMetric),
    metricId, metricLabels, metricName)
import qualified Control.Monad.JaegerMetrics.Class as M
import Control.Monad.JaegerTrace.Class (MonadJaegerTrace)

-- | The 'Metrics' structure contains all 'Counter's and 'Gauge's used by 'JaegerMetricsT'.
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

-- | Register 'Metrics' with a 'Store'.
--
-- /Caution:/ This may be called only once per store.
mkMetrics :: Store -> IO Metrics
mkMetrics store = Metrics <$> createCounter (metricName' M.TracesStartedSampled) store
                          <*> createCounter (metricName' M.TracesStartedNotSampled) store
                          <*> createCounter (metricName' M.TracesJoinedSampled) store
                          <*> createCounter (metricName' M.TracesJoinedNotSampled) store
                          <*> createCounter (metricName' M.SpansStarted) store
                          <*> createCounter (metricName' M.SpansFinished) store
                          <*> createCounter (metricName' M.SpansSampled) store
                          <*> createCounter (metricName' M.SpansNotSampled) store
                          <*> createGauge (metricName' M.ReporterQueueLength) store
                          <*> createCounter (metricName' M.ReporterSuccess) store
                          <*> createCounter (metricName' M.ReporterFailure) store
  where
    metricName' metric =
        let mid = metricId metric in
        Text.intercalate "." (metricName mid ++ map snd (metricLabels mid))

-- | Monad transformer to add 'MonadJaegerMetrics' functionality to a stack.
--
-- This implementation uses a 'Metrics' structure to keep 'Counter's and
-- 'Gauge's, and as such is meant to be used with @EKG@ reporting.
newtype JaegerMetricsT m a = JaegerMetricsT { unJaegerMetricsT :: ReaderT Metrics m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger,
        MonadJaegerTrace)

deriving instance MonadResource m => MonadResource (JaegerMetricsT m)

instance MonadReader r m => MonadReader r (JaegerMetricsT m) where
    ask = lift ask
    reader = lift . reader
    local = mapJaegerMetricsT . local

instance MonadRWS r w s m => MonadRWS r w s (JaegerMetricsT m)

instance MonadTransControl JaegerMetricsT where
    type StT JaegerMetricsT a = StT (ReaderT Metrics) a
    liftWith = defaultLiftWith JaegerMetricsT unJaegerMetricsT
    restoreT = defaultRestoreT JaegerMetricsT

instance MonadBaseControl b m => MonadBaseControl b (JaegerMetricsT m) where
    type StM (JaegerMetricsT m) a = ComposeSt JaegerMetricsT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadBase IO m => MonadJaegerMetrics (JaegerMetricsT m) where
    incMetric m = JaegerMetricsT $ ask >>= \metrics -> liftBase $ case m of
        M.TracesStartedSampled -> Counter.inc (metricsTracesStartedSampled metrics)
        M.TracesStartedNotSampled -> Counter.inc (metricsTracesStartedNotSampled metrics)
        M.TracesJoinedSampled -> Counter.inc (metricsTracesJoinedSampled metrics)
        M.TracesJoinedNotSampled -> Counter.inc (metricsTracesJoinedNotSampled metrics)
        M.SpansStarted -> Counter.inc (metricsSpansStarted metrics)
        M.SpansFinished -> Counter.inc (metricsSpansFinished metrics)
        M.SpansSampled -> Counter.inc (metricsSpansSampled metrics)
        M.SpansNotSampled -> Counter.inc (metricsSpansNotSampled metrics)
        M.ReporterQueueLength -> Gauge.inc (metricsReporterQueueLength metrics)
        M.ReporterSuccess -> Counter.inc (metricsReporterSuccess metrics)
        M.ReporterFailure -> Counter.inc (metricsReporterFailure metrics)

    addMetric m i = JaegerMetricsT $ ask >>= \metrics -> liftBase $ case m of
        M.TracesStartedSampled -> Counter.add (metricsTracesStartedSampled metrics) i'
        M.TracesStartedNotSampled -> Counter.add (metricsTracesStartedNotSampled metrics) i'
        M.TracesJoinedSampled -> Counter.add (metricsTracesJoinedSampled metrics) i'
        M.TracesJoinedNotSampled -> Counter.add (metricsTracesJoinedNotSampled metrics) i'
        M.SpansStarted -> Counter.add (metricsSpansStarted metrics) i'
        M.SpansFinished -> Counter.add (metricsSpansFinished metrics) i'
        M.SpansSampled -> Counter.add (metricsSpansSampled metrics) i'
        M.SpansNotSampled -> Counter.add (metricsSpansNotSampled metrics) i'
        M.ReporterQueueLength -> Gauge.add (metricsReporterQueueLength metrics) i'
        M.ReporterSuccess -> Counter.add (metricsReporterSuccess metrics) i'
        M.ReporterFailure -> Counter.add (metricsReporterFailure metrics) i'
      where
        i' :: Int64
        i' = fromIntegral i

    resetMetric m = JaegerMetricsT $ ask >>= \metrics -> liftBase $ case m of
        M.ReporterQueueLength -> Gauge.set (metricsReporterQueueLength metrics) 0

mapJaegerMetricsT :: (m a -> n b) -> JaegerMetricsT m a -> JaegerMetricsT n b
mapJaegerMetricsT f = JaegerMetricsT . mapReaderT f . unJaegerMetricsT

-- | Execute a 'JaegerMetricsT' action, collecting metrics in the given 'Metrics'.
runJaegerMetricsT :: JaegerMetricsT m a -> Metrics -> m a
runJaegerMetricsT = runReaderT . unJaegerMetricsT


-- | 'JaegerMetricsT' applied over 'IO', for ease-of-use.
type JaegerMetrics = JaegerMetricsT IO

-- | 'runJaegerMetricsT' for 'JaegerMetrics'.
runJaegerMetrics :: JaegerMetrics a -> Metrics -> IO a
runJaegerMetrics = runJaegerMetricsT


-- | Monad transformer which discards any metrics collection.
newtype NoJaegerMetricsT m a = NoJaegerMetricsT { unNoJaegerMetricsT :: IdentityT m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadReader r, MonadRWS r w s, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger,
        MonadJaegerTrace)

deriving instance MonadResource m => MonadResource (NoJaegerMetricsT m)

instance MonadTransControl NoJaegerMetricsT where
    type StT NoJaegerMetricsT a = StT IdentityT a
    liftWith = defaultLiftWith NoJaegerMetricsT unNoJaegerMetricsT
    restoreT = defaultRestoreT NoJaegerMetricsT

instance MonadBaseControl b m => MonadBaseControl b (NoJaegerMetricsT m) where
    type StM (NoJaegerMetricsT m) a = ComposeSt NoJaegerMetricsT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance Monad m => MonadJaegerMetrics (NoJaegerMetricsT m) where
    incMetric _ = pure ()
    addMetric _ _ = pure ()
    resetMetric _ = pure ()

-- | Evaluate a 'NoJaegerMetricsT' action, discarding any metrics collection.
runNoJaegerMetricsT :: NoJaegerMetricsT m a -> m a
runNoJaegerMetricsT = runIdentityT . unNoJaegerMetricsT


-- | 'NoJaegerMetricsT' applied over 'IO'.
type NoJaegerMetrics = NoJaegerMetricsT IO

-- | runNoJaegerMetricsT' for 'NoJaegerMetrics'.
runNoJaegerMetrics :: NoJaegerMetrics a -> IO a
runNoJaegerMetrics = runNoJaegerMetricsT
