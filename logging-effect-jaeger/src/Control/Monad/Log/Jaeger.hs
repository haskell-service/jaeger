{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      Control.Monad.Log.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: FlexibleContexts, OverloadedStrings, RankNTypes
--
-- A 'Control.Monad.Log.LoggingT' 'Handler' which emits log entries to
-- <https://uber.github.io/jaeger/ Jaeger> 'Jaeger.Types.Span's in a
-- 'MonadJaegerTrace' environment.
--
-- @
-- app :: MonadLog (WithCallStack (WithSeverity Doc)) m => m ()
-- app = logMessage $ withCallStack $ WithSeverity Informational "Informational message"
--
-- main :: IO ()
-- main = withJaeger $ \sock -> process >>== \proc ->
--     runNoJaegerMetricsT $ runJaegerT (runJaegerTraceT act "root") sock proc (constSampler True)
--   where
--     act = runLoggingT app (handler handleMessage)
--     -- handleMessage :: Renderer (WithCallStack (WithSeverity Doc))
--     handleMessage = handleCallStack $ handleSeverity $ handleDoc 0.4 80
-- @

module Control.Monad.Log.Jaeger (
    -- * 'Control.Monad.Log.LoggingT' 'Handler'
      handler
    , Renderer
    , WrapperRenderer
    -- * Message 'Renderer's
    , handleDoc
    , handleText
    , handleLazyText
    , discard
    -- * @With*@ 'WrapperRenderer's
    , handleCallStack
    , handleSeverity
    , handleTimestamp
    , discardWrapper
    ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Log (
    Handler,
    Severity(Alert, Critical, Debug, Emergency, Error, Informational, Notice, Warning),
    WithCallStack, discardCallStack, msgCallStack,
    WithSeverity, discardSeverity, msgSeverity,
    WithTimestamp, discardTimestamp, msgTimestamp)

import GHC.Stack (prettyCallStack)

import Control.Lens ((&), (%~), _1, _2)

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

import qualified Text.PrettyPrint.Leijen.Text as PP

import System.Clock (TimeSpec(TimeSpec))

import Jaeger.OpenTracing.Tags (logMessage, logStack)
import Jaeger.Types (Tag, log, spanLogs, stringTag)

import Control.Monad.JaegerTrace.Class (MonadJaegerTrace, addLog, modifyCurrentSpan)

-- | Construct a 'Handler' for @message@s, given a 'Renderer' for them.
--
-- The resulting 'Handler' will construct a 'Jaeger.Types.Log' entry and attach
-- it to the current 'Jaeger.Types.Span' in a 'MonadJaegerTrace' environment.
--
-- If any 'Renderer' yields a 'TimeSpec', this is used as the 'Log's timestamp.
-- Otherwise, 'addLog' is called, which will retrieve the current timestamp
-- (which may be slightly beyond the time at which the
-- 'Control.Monad.Log.MonadLog' effect was called).
handler :: (MonadBase IO m, MonadJaegerTrace m)
        => Renderer message
        -> Handler m message
handler fn message = case fn message of
    (Nothing, tags) -> addLog tags
    (Just ts, tags) -> modifyCurrentSpan () (\s -> ((), s & spanLogs %~ (Jaeger.Types.log ts tags:)))

-- | A @Renderer a@ turns an @a@ into a list of 'Tag's and 'Maybe' a 'TimeSpec'.
type Renderer a = a -> (Maybe TimeSpec, [Tag])
-- | A 'WrapperRenderer' for @w a@ is a partial 'Renderer' for @w@, delegating to another 'Renderer' for @a@.
--
-- 'WrapperRenderer's can be composed using plain function application ('$'), e.g.
--
-- > handleTimestamp $ handleSeverity $ handleDoc 0.4 80
--
-- forms a 'Renderer' for @'WithTimestamp' ('WithSeverity' 'PP.Doc')@.
type WrapperRenderer w = forall a. Renderer a -> Renderer (w a)

-- | Discard what's left.
discard :: Renderer a
discard _ = (Nothing, [])

-- | Discard a single wrapper using an unwrap function.
--
-- As an example,
--
-- > discardWrapper discardCallStack $ handleSeverity handleText
--
-- is a @'Renderer' ('WithCallStack' ('WithSeverity' 'Text'))@ which will not
-- attach any 'CallStack' information to generated 'Log's.
discardWrapper :: (forall a. w a -> a)  -- ^ Unwrap function
               -> WrapperRenderer w
discardWrapper fn inner msg = inner (fn msg)

-- | Handle 'WithTimestamp'.
--
-- If some other 'Renderer' already set a 'TimeSpec', the result of 'min'
-- between said 'TimeSpec' and the 'WithTimestamp' one is used.
handleTimestamp :: WrapperRenderer WithTimestamp
handleTimestamp inner msg = inner (discardTimestamp msg) & _1 %~ Just . maybe ts (min ts)
  where
    ts = let (secs, rest) = properFraction $ utcTimeToPOSIXSeconds $ msgTimestamp msg in
         TimeSpec secs (floor $ rest * 1000 * 1000 * 1000)

-- | Handle 'WithSeverity'.
--
-- A textual representation of the 'Severity' is attached to the 'Log' entry.
handleSeverity :: WrapperRenderer WithSeverity
handleSeverity inner msg = inner (discardSeverity msg) & _2 %~ (stringTag "level" level:)
  where
    level = case msgSeverity msg of
        Alert -> "alert"
        Critical -> "critical"
        Debug -> "debug"
        Emergency -> "emergency"
        Error -> "error"
        Informational -> "informational"
        Notice -> "notice"
        Warning -> "warning"

-- | Handle 'WithCallStack'.
--
-- A textual representation of the 'CallStack' is attached to the 'Log' entry
-- using 'logStack'.
handleCallStack :: WrapperRenderer WithCallStack
handleCallStack inner msg = inner (discardCallStack msg) & _2 %~ (logStack cs:)
  where
    cs = Text.pack $ prettyCallStack $ msgCallStack msg

-- | Render a 'PP.Doc' message.
--
-- The two arguments determine how this pretty-printing should be realised when
-- outputting log lines.
handleDoc :: Float  -- ^ The @ribbonFrac@ parameter to 'PP.renderPretty'
          -> Int  -- ^ The amount of characters per line. Lines longer than this will be pretty-printed across multiple lines if possible.
          -> Renderer PP.Doc
handleDoc ribbonFrac width msg = (Nothing, [logMessage $ PP.displayTStrict $ PP.renderPretty ribbonFrac width msg])

-- | Render a 'Text' message.
handleText :: Renderer Text
handleText msg = (Nothing, [logMessage msg])

-- | Render a 'Lazy.Text' message.
handleLazyText :: Renderer Lazy.Text
handleLazyText msg = (Nothing, [logMessage $ Lazy.toStrict msg])
