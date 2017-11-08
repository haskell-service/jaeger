{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Portability: DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, UndecidableInstances
--
-- Instances of 'MonadJaegerMetrics'.

module Control.Monad.Trans.JaegerMetrics (
    -- * 'InMemoryMetricsT' transformer
      InMemoryMetricsT
    , runInMemoryMetricsT
    -- ** 'InMemoryMetricsT' over 'IO'
    , InMemoryMetrics
    , runInMemoryMetrics
    -- * 'NoJaegerMetricsT' transformer
    , NoJaegerMetricsT
    , runNoJaegerMetricsT
    -- ** 'NoJaegerMetricsT' over 'IO'
    , NoJaegerMetrics
    , runNoJaegerMetrics
    ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState(get, put, state), modify)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(liftBaseWith, restoreM), StM, ComposeSt,
    defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(liftWith, restoreT), StT, defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Writer.Class (MonadWriter)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.Jaeger.Class (MonadJaeger)
import Control.Monad.JaegerMetrics.Class ( MonadJaegerMetrics(addMetric, incMetric, resetMetric))
import qualified Control.Monad.JaegerMetrics.Class as M
import Control.Monad.JaegerTrace.Class (MonadJaegerTrace)

data InMemoryMetricsState = InMemoryMetricsState { inMemoryMetricsCounters :: Map (M.Metric 'M.Counter) Word
                                                 , inMemoryMetricsGauges :: Map (M.Metric 'M.Gauge) Word
                                                 }
    deriving (Show, Eq)

-- | Monad transformer to add in-memory 'MonadJaegerMetrics' collection to a stack.
newtype InMemoryMetricsT m a = InMemoryMetricsT { unInMemoryMetricsT :: StateT InMemoryMetricsState m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadReader r, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger,
        MonadJaegerTrace)

instance MonadState s m => MonadState s (InMemoryMetricsT m) where
    get = lift get
    put = lift . put
    state = lift . state

instance MonadRWS r w s m => MonadRWS r w s (InMemoryMetricsT m)
deriving instance MonadResource m => MonadResource (InMemoryMetricsT m)

instance MonadTransControl InMemoryMetricsT where
    type StT InMemoryMetricsT a = StT (StateT InMemoryMetricsState) a
    liftWith = defaultLiftWith InMemoryMetricsT unInMemoryMetricsT
    restoreT = defaultRestoreT InMemoryMetricsT

instance MonadBaseControl b m => MonadBaseControl b (InMemoryMetricsT m) where
    type StM (InMemoryMetricsT m) a = ComposeSt InMemoryMetricsT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance Monad m => MonadJaegerMetrics (InMemoryMetricsT m) where
    incMetric = flip addMetric 1
    addMetric m i = InMemoryMetricsT $ modify $ \(InMemoryMetricsState c g) -> case m of
        M.ReporterQueueLength -> InMemoryMetricsState c (Map.alter (Just . maybe i (+ i)) M.ReporterQueueLength g)
        M.TracesStartedSampled -> incCounter c g m
        M.TracesStartedNotSampled -> incCounter c g m
        M.TracesJoinedSampled -> incCounter c g m
        M.TracesJoinedNotSampled -> incCounter c g m
        M.SpansStarted -> incCounter c g m
        M.SpansFinished -> incCounter c g m
        M.SpansSampled -> incCounter c g m
        M.SpansNotSampled -> incCounter c g m
        M.ReporterSuccess -> incCounter c g m
        M.ReporterFailure -> incCounter c g m
      where
        incCounter c g m' = InMemoryMetricsState (Map.alter (Just . maybe i (+ i)) m' c) g
    resetMetric m = InMemoryMetricsT $ modify $ \(InMemoryMetricsState c g) -> case m of
        M.ReporterQueueLength -> InMemoryMetricsState c (Map.insert m 0 g)

-- | Evaluate an 'InMemoryMetricsT' action, collecting metrics in in-memory 'Map's.
runInMemoryMetricsT :: Monad m => InMemoryMetricsT m a -> m (a, (Map (M.Metric 'M.Counter) Word, Map (M.Metric 'M.Gauge) Word))
runInMemoryMetricsT act = do
    (r, ms) <- runStateT (unInMemoryMetricsT act) $ InMemoryMetricsState mempty mempty
    return (r, (inMemoryMetricsCounters ms, inMemoryMetricsGauges ms))


-- | 'InMemoryMetricsT' applied over 'IO'.
type InMemoryMetrics = InMemoryMetricsT IO

-- | 'runInMemoryMetricsT' for 'InMemoryMetrics'.
runInMemoryMetrics :: InMemoryMetrics a -> IO (a, (Map (M.Metric 'M.Counter) Word, Map (M.Metric 'M.Gauge) Word))
runInMemoryMetrics = runInMemoryMetricsT


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
