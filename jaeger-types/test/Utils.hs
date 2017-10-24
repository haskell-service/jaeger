{-# LANGUAGE MultiWayIf #-}

module Utils (
      _Tag
    , _Log
    , _SpanRef
    , _Span
    , _Process
    , _Batch
    ) where

import Data.Bits ((.|.), testBit)
import Data.Int (Int64)

import Control.Lens (
    Iso',
    (^.), (^..), (^?!),
    _2, _Empty, _Just, _Unwrapped, _Wrapped,
    each, from, folded, filtered, iso, lazy, mapping, non, non', re, strict)
import Data.Vector.Lens (vector)

import System.Clock (TimeSpec, fromNanoSecs, toNanoSecs)

import Data.Set.Lens (setOf)

import qualified Jaeger.Types as J
import qualified Jaeger_Types as T

_Tag :: Iso' T.Tag J.Tag
_Tag = iso
    (\(T.Tag k t s d b l b') ->
        let k' = k ^. strict in
        case t of
            T.STRING -> J.stringTag k' (s ^?! _Just . strict)
            T.DOUBLE -> J.doubleTag k' (d ^?! _Just)
            T.BOOL -> J.boolTag k' (b ^?! _Just)
            T.LONG -> J.longTag k' (l ^?! _Just)
            T.BINARY -> J.binaryTag k' (b' ^?! _Just . strict))
    (\t ->
         let k' = t ^. J.tagKey . lazy in
         J.mapTagValue
             (\s -> T.Tag k' T.STRING (Just $ s ^. lazy) Nothing Nothing Nothing Nothing)
             (\d -> T.Tag k' T.DOUBLE Nothing (Just d) Nothing Nothing Nothing)
             (\b -> T.Tag k' T.BOOL Nothing Nothing (Just b) Nothing Nothing)
             (\l -> T.Tag k' T.LONG Nothing Nothing Nothing (Just l) Nothing)
             (\b -> T.Tag k' T.BINARY Nothing Nothing Nothing Nothing (Just $ b ^. lazy))
             t)

_TimeSpecUS :: Iso' Int64 TimeSpec
_TimeSpecUS = iso
    (fromNanoSecs . fromIntegral . (* 1000))
    ((`div` 1000) . fromInteger . toNanoSecs)

_Log :: Iso' T.Log J.Log
_Log = iso
    (\(T.Log t f) -> J.log
        (t ^. _TimeSpecUS)
        (f ^.. each . _Tag))
    (\l -> T.Log
        (l ^. J.logTimestamp . re _TimeSpecUS)
        (l ^. J.logFields . mapping (from _Tag) . vector))

_SpanRef :: Iso' T.SpanRef J.SpanRef
_SpanRef = iso
    (\(T.SpanRef t l h s) -> J.spanRef
        (case t of
            T.CHILD_OF -> J.childOf
            T.FOLLOWS_FROM -> J.followsFrom)
        (J.traceId l h)
        (s ^. _Unwrapped))
    (\s -> T.SpanRef
        (if | s ^. J.spanRefType == J.childOf -> T.CHILD_OF
            | s ^. J.spanRefType == J.followsFrom -> T.FOLLOWS_FROM
            | otherwise -> error "Unknown SpanRef type")
        (s ^. J.spanRefTraceId . J.traceIdLow)
        (s ^. J.spanRefTraceId . J.traceIdHigh)
        (s ^. J.spanRefSpanId . _Wrapped))

_Span :: Iso' T.Span J.Span
_Span = iso
    (\(T.Span l h s p o r f st d ts ls) -> J.span
        (J.traceId l h)
        (s ^. _Unwrapped)
        (p ^. _Unwrapped . re _Just)
        (o ^. strict)
        (r ^. _Just . mapping _SpanRef . re vector)
        (setOf (folded . filtered (testBit f . fst) . _2) [ (0, J.sampled)
                                                          , (1, J.debug)
                                                          ])
        (st ^. _TimeSpecUS)
        (d ^. _TimeSpecUS)
        (ts ^. _Just . mapping _Tag . re vector)
        (ls ^. _Just . mapping _Log . re vector))
    (\s -> T.Span
        (s ^. J.spanTraceId . J.traceIdLow)
        (s ^. J.spanTraceId . J.traceIdHigh)
        (s ^. J.spanSpanId . _Wrapped)
        (s ^. J.spanParentSpanId . non (0 ^. _Unwrapped) . _Wrapped)
        (s ^. J.spanOperationName . lazy)
        (s ^. J.spanReferences . mapping (from _SpanRef) . vector . re (non' _Empty))
        (foldr (\a b -> b .|. if | a == J.sampled -> 1
                                 | a == J.debug -> 2
            ) 0 (s ^. J.spanFlags))
        (s ^. J.spanStartTime . re _TimeSpecUS)
        (s ^. J.spanDuration . re _TimeSpecUS)
        (s ^. J.spanTags . mapping (from _Tag) . vector . re (non' _Empty))
        (s ^. J.spanLogs . mapping (from _Log) . vector . re (non' _Empty)))

_Process :: Iso' T.Process J.Process
_Process = iso
    (\(T.Process s t) -> J.process
        (s ^. strict)
        (t ^.. _Just . each . _Tag))
    (\p -> T.Process
        (p ^. J.processServiceName . lazy)
        (p ^. J.processTags . mapping (from _Tag) . vector . re (non' _Empty)))

_Batch :: Iso' T.Batch J.Batch
_Batch = iso
    (\(T.Batch p s) -> J.batch
        (p ^. _Process)
        (s ^.. each . _Span))
    (\b -> T.Batch
        (b ^. J.batchProcess . re _Process)
        (b ^. J.batchSpans . mapping (from _Span) . vector))
