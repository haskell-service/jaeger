{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:      Control.Monad.Trans.JaegerTrace
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, OverloadedLists, StandaloneDeriving, TypeFamilies, UndecidableInstances
--
-- Instances of 'MonadJaegerTrace'.

module Control.Monad.Trans.JaegerTrace (
    -- * 'JaegerTraceT' transformer
      JaegerTraceT
    , runJaegerTraceT
    , continueJaegerTraceT
    , forkJaegerTraceT
    -- * 'NoJaegerTraceT' transformer
    , NoJaegerTraceT
    , runNoJaegerTraceT
    -- ** 'NoJaegerTraceT' over 'IO'
    , NoJaegerTrace
    , runNoJaegerTrace
    ) where

import Data.IORef (IORef)
import Data.Maybe (fromMaybe)

import Control.Concurrent (ThreadId)
import Control.Concurrent.Lifted (fork)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState, get, put, state)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Identity (IdentityT, runIdentityT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, finally, withException)

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Set as Set

import Data.Text (Text)

import Control.Lens ((&), (.~), _Unwrapped, view)

import System.Random (randomIO)

import Control.Monad.Trans.State.Ref (StateRefT, runStateIORefT)

import Jaeger.Types (
    Span, SpanContext, SpanRefType,
    childOf, debug, inject, sampled, span', spanContext, spanContextFlags,
    spanContextSpanId, spanContextTraceId, spanDuration, spanFlags, spanRef,
    spanReferences, spanSpanId, spanStartTime, spanTraceId, traceId)

import Jaeger.Clock (TimeStamp, diffTimeStamp, monotonicTime, wallClockTime)
import Control.Monad.Jaeger.Class (MonadJaeger, emitSpan, sample)
import Control.Monad.JaegerMetrics.Class (MonadJaegerMetrics, incMetric)
import qualified Control.Monad.JaegerMetrics.Class as M
import Control.Monad.JaegerTrace.Class (
    MonadJaegerTrace(captureSpanContext, endCurrentSpan, modifyCurrentSpan, startSpan),
    addTags, reportException)

data JaegerTraceTState = NoTrace !SpanContext
                       | Trace !(NonEmpty (TimeStamp, Span))

-- | Monad transformer to add 'MonadJaegerTrace' functionalities to a stack including 'MonadJaeger'.
--
-- /Note:/ Incorrect nesting of 'startSpan' and 'endCurrentSpan' calls results
-- in exceptions being thrown. Prefer using
-- 'Control.Monad.JaegerTrace.Class.inSpan'.
--
-- /Note:/ This transformer uses mutuable state under the hood to track the
-- current stack of 'Span's, hence care should be taken not to capture and share
-- this state incorrectly.
newtype JaegerTraceT m a = JaegerTraceT { unJaegerTraceT :: StateRefT IORef JaegerTraceTState m a }
    deriving (
        Functor, Applicative, Monad,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b)

instance MonadJaeger m => MonadJaeger (JaegerTraceT m)
instance MonadJaegerMetrics m => MonadJaegerMetrics (JaegerTraceT m)
deriving instance MonadResource m => MonadResource (JaegerTraceT m)

instance (MonadJaeger m, MonadJaegerMetrics m, MonadBase IO m) => MonadJaegerTrace (JaegerTraceT m) where
    startSpan operationName = JaegerTraceT $ do
        incMetric M.SpansStarted

        get >>= \case
            NoTrace{} ->
                incMetric M.SpansNotSampled

            Trace stack -> do
                incMetric M.SpansSampled

                (sid, startTime, begin) <- liftBase $ (,,) <$> randomIO <*> wallClockTime <*> monotonicTime
                let hd = snd $ NonEmpty.head stack
                    tid = view spanTraceId hd
                    psid = view spanSpanId hd
                    sp = span' tid sid (Just psid) operationName
                            & spanStartTime .~ startTime
                            & spanReferences .~ [spanRef childOf tid psid]
                            & spanFlags .~ view spanFlags hd

                put $ Trace $ NonEmpty.cons (begin, sp) stack

    endCurrentSpan = do
        tos <- JaegerTraceT $ state $ \case
            nt@NoTrace{} -> (Nothing, nt)
            Trace stack ->
                let (hd, tl) = NonEmpty.uncons stack in
                let tl' = fromMaybe (error "Invariant violation: span stack underflow") tl in
                (Just hd, Trace tl')
        maybe (pure ())
              (\(begin, s) -> do
                  end <- liftBase monotonicTime
                  emitSpan $ s & spanDuration .~ diffTimeStamp end begin)
              tos
        incMetric M.SpansFinished

    modifyCurrentSpan a f = JaegerTraceT $ state $ \case
        nt@NoTrace{} -> (a, nt)
        Trace ((ts, sp) :| tl) ->
            let (a', sp') = f sp in
            sp' `seq` (a', Trace $ (ts, sp') :| tl)

    captureSpanContext = JaegerTraceT $ get >>= \case
        NoTrace sc -> pure $ inject sc
        Trace ((_, sp) :| _) -> pure $ inject sp

-- | Run a 'JaegerTraceT' action, as a root trace.
--
-- /Note:/ This trace is metered as a /started/ trace.
runJaegerTraceT :: (MonadBase IO m, MonadMask m, MonadJaeger m, MonadJaegerMetrics m)
                => JaegerTraceT m a
                -> Text  -- ^ Root span 'Jaeger.Types.spanOperationName'
                -> m a
runJaegerTraceT act operationName = do
    tid <- liftBase randomIO
    (doSample, tags) <- sample tid operationName
    let span0 = view _Unwrapped 0
        flags = Set.fromList [sampled | doSample]
        ctx = spanContext tid span0 span0 flags
    incMetric $ if doSample then M.TracesStartedSampled else M.TracesStartedNotSampled
    runJaegerTraceTNoMetrics (addTags tags >> act) operationName childOf ctx

-- | Run a 'JaegerTraceT' action as part of a running trace.
--
-- /Note:/ This trace is metered as a /joined/ trace.
continueJaegerTraceT :: (MonadBase IO m, MonadMask m, MonadJaeger m, MonadJaegerMetrics m)
                     => JaegerTraceT m a
                     -> Text  -- ^ Root span of the given action's 'Jaeger.Types.spanOperationName'
                     -> SpanRefType  -- ^ Kind of reference to the given 'SpanContext'
                     -> SpanContext  -- ^ 'SpanContext' of the current trace and this action's 'Span' parent 'Span'
                     -> m a
continueJaegerTraceT act operationName refType ctx = do
    incMetric $ if shouldSample ctx then M.TracesJoinedSampled else M.TracesJoinedNotSampled
    runJaegerTraceTNoMetrics act operationName refType ctx

shouldSample :: SpanContext -> Bool
shouldSample ctx = Set.member sampled flags || Set.member debug flags
  where
    flags = view spanContextFlags ctx

runJaegerTraceTNoMetrics :: (MonadBase IO m, MonadMask m, MonadJaeger m, MonadJaegerMetrics m)
                         => JaegerTraceT m a
                         -> Text
                         -> SpanRefType
                         -> SpanContext
                         -> m a
runJaegerTraceTNoMetrics act operationName refType ctx
    | shouldSample ctx = runTrace
    | otherwise = noTrace
  where
    runTrace = do
        root <- liftBase $ do
            startTime <- wallClockTime
            begin <- monotonicTime
            sid <- randomIO
            let tid = view spanContextTraceId ctx
                psid = view spanContextSpanId ctx
                rootSpan = span' tid sid (if refType == childOf then Just psid else Nothing) operationName
                            & spanStartTime .~ startTime
                            & spanFlags .~ view spanContextFlags ctx
                            & spanReferences .~ [spanRef refType tid psid]
            pure (begin, rootSpan)

        fst <$> runStateIORefT (unJaegerTraceT $ act `withException` reportException `finally` cleanup) (Trace [root])

    cleanup = JaegerTraceT get >>= \case
        NoTrace{} -> pure ()
        Trace stack -> case stack of
            (start, root) :| [] -> do
                end <- liftBase monotonicTime
                emitSpan $ root & spanDuration .~ diffTimeStamp end start
            _ -> error "Invariant violation: leftover spans"

    noTrace = fst <$> runStateIORefT (unJaegerTraceT act) (NoTrace ctx)

-- | Fork a 'JaegerTraceT' action.
--
-- This will run the given action in a new thread (using 'fork'), in a new
-- 'Span', part of the current trace, with a reference to the current 'Span'.
--
-- /Note:/ Unlike when using 'runJaegerTraceT' or 'continueJaegerTraceT', this
-- action will not appear as a new traced span in the metrics.
--
-- /Caution:/ Beware of how 'MonadBaseControl' and 'fork' behave with
-- 'Control.Monad.State.State'-like base monads!
forkJaegerTraceT :: (MonadBaseControl IO m, MonadMask m, MonadJaeger m, MonadJaegerMetrics m)
                 => JaegerTraceT m ()  -- ^ Action to run in a new thread
                 -> Text  -- ^ Root span of the given action's 'Jaeger.Types.spanOperationName'
                 -> SpanRefType  -- ^ Kind of reference to the current 'SpanContext'
                 -> JaegerTraceT m ThreadId
forkJaegerTraceT act operationName refType = do
    sc <- captureSpanContext
    lift $ fork $
        runJaegerTraceTNoMetrics act operationName refType sc


-- | Monad transformer which doesn't capture any tracing.
newtype NoJaegerTraceT m a = NoJaegerTraceT { unNoJaegerTraceT :: IdentityT m a }
    deriving (
        Functor, Applicative, Monad,
        MonadCont, MonadError e, MonadReader r, MonadRWS r w s, MonadState s, MonadWriter w,
        MonadTrans,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger,
        MonadJaegerMetrics)

deriving instance MonadResource m => MonadResource (NoJaegerTraceT m)

instance Monad m => MonadJaegerTrace (NoJaegerTraceT m) where
    startSpan _ = pure ()
    endCurrentSpan = pure ()
    modifyCurrentSpan a _ = pure a
    captureSpanContext = pure $ inject $ spanContext (traceId 0 0) span0 span0 []
      where
        span0 = view _Unwrapped 0

-- | Run a 'NoJaegerTraceT' action, discarding any 'MonadJaegerTrace' effects.
runNoJaegerTraceT :: NoJaegerTraceT m a -> m a
runNoJaegerTraceT = runIdentityT . unNoJaegerTraceT

-- | 'NoJaegerTraceT' over 'IO'.
type NoJaegerTrace = NoJaegerTraceT IO

-- | 'runNoJaegerTraceT' for 'NoJaegerTrace'.
runNoJaegerTrace :: NoJaegerTrace a -> IO a
runNoJaegerTrace = runNoJaegerTraceT
