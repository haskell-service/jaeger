{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:      Control.Monad.Trans.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, UndecidableInstances
--
-- Instances of 'MonadJaeger'.

module Control.Monad.Trans.Jaeger (
    -- * 'JaegerT' transformer
      JaegerT
    , runJaegerT
    , getProcess
    , getSampler
    , getSocket
    -- * 'NoJaegerT' transformer
    , NoJaegerT
    , runNoJaegerT
    -- ** 'NoJaegerT' over 'IO'
    , NoJaeger
    , runNoJaeger
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, swapTVar, writeTVar)
import Data.List (genericLength)
import Data.Monoid (Sum(Sum, getSum))
import Data.Word (Word)

import Network.Socket (Socket)

import Control.Monad (join, unless)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader(ask, reader, local), asks)
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

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, catchIO, finally)

import Control.Lens ((^.))

import qualified Data.ByteString as BS
import qualified Data.Text as Text

import Jaeger.Sampler (Sampler)
import Jaeger.Types (
    Process, Span, batch, logFields, mapTagValue,
    spanLogs, spanOperationName, spanReferences, spanTags,
    tagKey)
import Network.Jaeger (sendBatch)

import Control.Monad.Jaeger.Class (MonadJaeger(emitSpan, sample))
import Control.Monad.JaegerMetrics.Class (
    MonadJaegerMetrics, addMetric, incMetric, resetMetric)
import qualified Control.Monad.JaegerMetrics.Class as M
import Control.Monad.JaegerTrace.Class (MonadJaegerTrace)
import Network.Socket.MTU (getMTU)

data Env = Env { envSocket :: !Socket
               , envMTU :: !Word
               , envProcess:: !Process
               , envSampler :: !(Sampler IO)
               , envQueue :: !(TVar (Word, [Span]))
               }

-- | Monad transformer to add 'MonadJaeger' functionalities to a stack.
newtype JaegerT m a = JaegerT { unJaegerT :: ReaderT Env m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaegerMetrics,
        MonadJaegerTrace)

deriving instance MonadResource m => MonadResource (JaegerT m)

instance MonadReader r m => MonadReader r (JaegerT m) where
    ask = lift ask
    reader = lift . reader
    local = mapJaegerT . local

instance MonadRWS r w s m => MonadRWS r w s (JaegerT m)

instance MonadTransControl JaegerT where
    type StT JaegerT a = StT (ReaderT Env) a
    liftWith = defaultLiftWith JaegerT unJaegerT
    restoreT = defaultRestoreT JaegerT

instance MonadBaseControl b m => MonadBaseControl b (JaegerT m) where
    type StM (JaegerT m) a = ComposeSt JaegerT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadBase IO m, MonadCatch m, MonadJaegerMetrics m) => MonadJaeger (JaegerT m) where
    emitSpan s = do
        env <- JaegerT ask
        -- Make sure it's evaluated before entering a 'critical section'. Not
        -- sure this is strictly required.
        estimatedSpanSize `seq` join $ liftBase $ atomically $ do
            (size, spans) <- readTVar (envQueue env)
            let newSize = size + estimatedSpanSize
            if newSize >= (envMTU env * 4) `div` 5 -- Fill up to 80%, to have some margin
                then do
                    let q = (estimatedSpanSize, [s])
                    q `seq` writeTVar (envQueue env) q
                    pure $
                        flushQueue env spans
                else do
                    let q = (newSize, s : spans)
                    q `seq` writeTVar (envQueue env) q
                    pure $
                        incMetric M.ReporterQueueLength
      where
        estimatedSpanSize = fromIntegral $
              -- length $ encode compactProtocol (span' (traceId 0 0) (review _Wrapped 0) (review _Wrapped 0) "")
              17
            + Text.length (s ^. spanOperationName)
              -- length $ encode compactProtocol (spanRef childOf (traceId 0 0) (review _Wrapped 0))
            + 9 * length (s ^. spanReferences)
            + getSum (foldMap (Sum . estimateTagSize) (s ^. spanTags))
            + getSum (foldMap (Sum . estimateLogSize) (s ^. spanLogs))
        estimateTagSize t =
              5
            + Text.length (t ^. tagKey)
            + mapTagValue ((+ 2) . Text.length)
                          (const 9) -- Seems to be 14
                          (const 1) -- Seems to be 6
                          (const 9) -- vint-encoded or so, so depends... Just picked something
                          ((+ 2) . BS.length)
                          t
        estimateLogSize l =
              5
            + getSum (foldMap (Sum . estimateTagSize) (l ^. logFields))

    sample tid op = do
        s <- JaegerT $ asks envSampler
        liftBase $ s tid op

mapJaegerT :: (m a -> n b) -> JaegerT m a -> JaegerT n b
mapJaegerT f = JaegerT . mapReaderT f . unJaegerT

-- | Execute a 'JaegerT' action, sending any emitted 'Span's to the given 'Socket' as generated by the given 'Process'.
runJaegerT :: (MonadBase IO m, MonadMask m, MonadJaegerMetrics m) => JaegerT m a -> Socket -> Process -> Sampler IO -> m a
runJaegerT act s p r = do
    (tv, mtu) <- liftBase $ (,) <$> newTVarIO (0, []) <*> getMTU s
    let env = Env s mtu p r tv
    runReaderT (unJaegerT act) env `finally` do
        (_, spans) <- liftBase $ atomically $ swapTVar (envQueue env) (0, [])
        flushQueue env spans

flushQueue :: (MonadBase IO m, MonadCatch m, MonadJaegerMetrics m) => Env -> [Span] -> m ()
flushQueue env spans = do
    unless (null spans) $
        catchIO
            (do
                liftBase $ sendBatch (envSocket env) $ batch (envProcess env) spans
                addMetric M.ReporterSuccess (genericLength spans))
            (\_ -> addMetric M.ReporterFailure (genericLength spans))
    resetMetric M.ReporterQueueLength

-- | Get the current 'JaegerT's 'Socket'.
--
-- /Note:/ The lifetime of this 'Socket' is not managed by the 'JaegerT' action.
getSocket :: Monad m => JaegerT m Socket
getSocket = JaegerT $ asks envSocket

-- | Get the current 'JaegerT' 'Process'.
getProcess :: Monad m => JaegerT m Process
getProcess = JaegerT $ asks envProcess

-- | Get the current 'JaegerT' 'Sampler'.
getSampler :: Monad m => JaegerT m (Sampler IO)
getSampler = JaegerT $ asks envSampler


-- | Monad transformer which discards any 'emitSpan' calls.
newtype NoJaegerT m a = NoJaegerT { unNoJaegerT :: IdentityT m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadReader r, MonadRWS r w s, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaegerMetrics,
        MonadJaegerTrace)

deriving instance MonadResource m => MonadResource (NoJaegerT m)

instance MonadTransControl NoJaegerT where
    type StT NoJaegerT a = StT IdentityT a
    liftWith = defaultLiftWith NoJaegerT unNoJaegerT
    restoreT = defaultRestoreT NoJaegerT

instance MonadBaseControl b m => MonadBaseControl b (NoJaegerT m) where
    type StM (NoJaegerT m) a = ComposeSt NoJaegerT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance Monad m => MonadJaeger (NoJaegerT m) where
    emitSpan _ = pure ()
    sample _ _ = pure (False, [])

-- | Evaluate a 'NoJaegerT' action, discarding any 'emitSpan' calls.
runNoJaegerT :: NoJaegerT m a -> m a
runNoJaegerT = runIdentityT . unNoJaegerT

-- | 'NoJaegerT' applied over 'IO'.
type NoJaeger = NoJaegerT IO

-- | 'runNoJaegerT' for 'NoJaeger'.
runNoJaeger :: NoJaeger a -> IO a
runNoJaeger = runNoJaegerT
