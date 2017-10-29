{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Control.Monad.JaegerTrace.Class
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DefaultSignatures, TypeFamilies
--
-- @mtl@-style class of monads which can capture stacks of Jaeger 'Span's.

module Control.Monad.JaegerTrace.Class (
      MonadJaegerTrace(..)
    -- * Utilities
    , inSpan
    , addLog, addTag
    , reportException
    , currentSpan
    , captureSpanContext
    ) where

import Data.Typeable (tyConName, typeOf, typeRepTyCon)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Monad.State (State, runState)

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
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

import Control.Lens ((%=))

import Control.Exception.Safe (
    MonadMask, SomeAsyncException(SomeAsyncException),
    SomeException(SomeException),
    displayException, finally, fromException, withException)

import Data.Text (Text)
import qualified Data.Text as Text

import Jaeger.Types (Carrier, Span, Tag, inject, log, spanLogs, spanTags)
import qualified Jaeger.OpenTracing.Tags as Tags

import Jaeger.Clock (wallClockTime)

class Monad m => MonadJaegerTrace m where
    -- | Enter a new span, child of whatever the current span is.
    startSpan :: Text -> m ()
    default startSpan :: (MonadJaegerTrace m', MonadTrans t, m ~ t m') => Text -> m ()
    startSpan = lift . startSpan

    -- | Finalize the current span.
    endCurrentSpan :: m ()
    default endCurrentSpan :: (MonadJaegerTrace m', MonadTrans t, m ~ t m') => m ()
    endCurrentSpan = lift endCurrentSpan

    -- | Modify the current span, if any.
    modifyCurrentSpan :: a  -- ^ Value to return when no trace is going on
                      -> (Span -> (a, Span))  -- ^ Function to apply to the current span
                      -> m a
    default modifyCurrentSpan :: (MonadJaegerTrace m', MonadTrans t, m ~ t m') => a -> (Span -> (a, Span)) -> m a
    modifyCurrentSpan a f = lift $ modifyCurrentSpan a f

instance MonadJaegerTrace m => MonadJaegerTrace (ContT r m)
instance MonadJaegerTrace m => MonadJaegerTrace (ExceptT e m)
instance MonadJaegerTrace m => MonadJaegerTrace (IdentityT m)
instance MonadJaegerTrace m => MonadJaegerTrace (ListT m)
instance MonadJaegerTrace m => MonadJaegerTrace (MaybeT m)
instance (Monoid w, MonadJaegerTrace m) => MonadJaegerTrace (Lazy.RWST r w s m)
instance (Monoid w, MonadJaegerTrace m) => MonadJaegerTrace (Strict.RWST r w s m)
instance MonadJaegerTrace m => MonadJaegerTrace (ReaderT r m)
instance MonadJaegerTrace m => MonadJaegerTrace (ResourceT m)
instance MonadJaegerTrace m => MonadJaegerTrace (Lazy.StateT s m)
instance MonadJaegerTrace m => MonadJaegerTrace (Strict.StateT s m)
instance (Monoid w, MonadJaegerTrace m) => MonadJaegerTrace (Lazy.WriterT w m)
instance (Monoid w, MonadJaegerTrace m) => MonadJaegerTrace (Strict.WriterT w m)

modifyCurrentSpan' :: MonadJaegerTrace m => State Span () -> m ()
modifyCurrentSpan' = modifyCurrentSpan () . runState

-- | Capture the current 'Span'.
--
-- @Nothing@ if there's no tracing going on.
currentSpan :: MonadJaegerTrace m => m (Maybe Span)
currentSpan = modifyCurrentSpan Nothing (\s -> (Just s, s))

-- | Add a 'Tag' to the current 'Span'.
addTag :: MonadJaegerTrace m => Tag -> m ()
addTag t = modifyCurrentSpan' $ spanTags %= (t:)

-- | Add a 'Jaeger.Types.Log' entry to the current 'Span'.
--
-- The 'Jaeger.Types.Log' 'Jaeger.Types.logTimestamp' is set to the current 'wallClockTime'.
addLog :: (MonadIO m, MonadJaegerTrace m) => [Tag] -> m ()
addLog tags = do
    entry <- flip Jaeger.Types.log tags <$> liftIO wallClockTime
    modifyCurrentSpan' $ spanLogs %= (entry:)

-- | Run an action inside a new span, capturing exceptions.
--
-- This runs the given action wrapped between 'startSpan' and 'endCurrentSpan',
-- whilst also capturing any exceptions and attaching related meta-information
-- to the current 'Span' using 'reportException'.
inSpan :: (MonadMask m, MonadIO m, MonadJaegerTrace m)
       => Text  -- ^ Operation name
       -> m a  -- ^ Action
       -> m a
inSpan o a = do
    startSpan o
    a `withException` reportException
      `finally` endCurrentSpan

-- | Modify the current 'Span', attaching meta-information of the given exception to it.
--
-- The 'spanTags' and 'spanLogs' fields added to the current 'Span' are based on
-- the OpenTracing specification.
reportException :: (MonadIO m, MonadJaegerTrace m) => SomeException -> m ()
reportException exc = do
    addTag (Tags.error True)

    let (msg, kind) = case fromException exc of
            Just (SomeAsyncException exc') -> (displayException exc', tyConName $ typeRepTyCon $ typeOf exc')
            Nothing -> case exc of
                SomeException exc' -> (displayException exc', tyConName $ typeRepTyCon $ typeOf exc')

    addLog [ Tags.logError
           , Tags.logErrorKind $ Text.pack kind
           , Tags.logMessage $ Text.pack msg
           ]

-- | Capture the current 'Span' as a serialized 'Jaeger.Types.SpanContext'.
--
-- @Nothing@ if there's no tracing going on. A client could map this to whatever
-- an /empty/ 'Jaeger.Types.SpanContext' should look like.
captureSpanContext :: (MonadJaegerTrace m, Carrier c) => m (Maybe c)
captureSpanContext = fmap inject <$> currentSpan
