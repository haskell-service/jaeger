{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module:      Control.Monad.JaegerMetrics.Class
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, DefaultSignatures, GADTs, KindSignatures, StandaloneDeriving
--
-- @mtl@-style class of monads which can collect Jaeger tracing metrics.

module Control.Monad.JaegerMetrics.Class (
      MonadJaegerMetrics(..)
    , Metric(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import Control.Monad.Trans.State.Ref (StateRefT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

-- | A kind of 'Metric'.
data MetricKind = Counter | Gauge

-- | Enumeration of all collected metrics.
data Metric (k :: MetricKind) where
    -- | Number of traces started by this tracer as sampled.
    TracesStartedSampled :: Metric 'Counter
    -- | Number of traces started by this tracer as not sampled.
    TracesStartedNotSampled :: Metric 'Counter
    -- | Number of externally started sampled traces this tracer joined.
    TracesJoinedSampled :: Metric 'Counter
    -- | Number of externally started not-sampled traces this tracer joined.
    TracesJoinedNotSampled :: Metric 'Counter
    -- | Number of sampled spans started by this tracer.
    SpansStarted :: Metric 'Counter
    -- | Number of sampled spans finished by this tracer.
    SpansFinished :: Metric 'Counter
    -- | Number of sampled spans started by this tracer.
    SpansSampled :: Metric 'Counter
    -- | Number of not-sampled spans started by this tracer.
    SpansNotSampled :: Metric 'Counter
    -- | Current number of spans in the reporter queue.
    ReporterQueueLength :: Metric 'Gauge
    -- | Number of spans successfully reported.
    ReporterSuccess :: Metric 'Counter
    -- | Number of spans in failed attempts to report.
    ReporterFailure :: Metric 'Counter

deriving instance Eq (Metric k)
deriving instance Show (Metric k)

class Monad m => MonadJaegerMetrics m where
    -- | Increase a 'Metric' by one.
    incMetric :: Metric k -> m ()
    default incMetric :: (MonadJaegerMetrics m', MonadTrans t, m ~ t m') => Metric k -> m ()
    incMetric = lift . incMetric

    -- | Increase a 'Metric' by the given amount.
    addMetric :: Metric k -> Word -> m ()
    default addMetric :: (MonadJaegerMetrics m', MonadTrans t, m ~ t m') => Metric k -> Word -> m ()
    addMetric m i = lift $ addMetric m i

    -- | Reset a 'Metric' to zero.
    resetMetric :: Metric 'Gauge -> m ()
    default resetMetric :: (MonadJaegerMetrics m', MonadTrans t, m ~ t m') => Metric 'Gauge -> m ()
    resetMetric = lift . resetMetric

instance MonadJaegerMetrics m => MonadJaegerMetrics (ContT r m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (ExceptT e m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (IdentityT m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (ListT m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (MaybeT m)
instance (Monoid w, MonadJaegerMetrics m) => MonadJaegerMetrics (Lazy.RWST r w s m)
instance (Monoid w, MonadJaegerMetrics m) => MonadJaegerMetrics (Strict.RWST r w s m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (ReaderT r m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (ResourceT m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (StateRefT ref s m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (Lazy.StateT s m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (Strict.StateT s m)
instance (Monoid w, MonadJaegerMetrics m) => MonadJaegerMetrics (Lazy.WriterT w m)
instance (Monoid w, MonadJaegerMetrics m) => MonadJaegerMetrics (Strict.WriterT w m)
