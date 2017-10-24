{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jaeger.Types.Span (
      Span
    , Jaeger.Types.Span.span, span'
    , spanTraceId, spanSpanId, spanParentSpanId, spanOperationName
        , spanReferences, spanFlags, spanStartTime, spanDuration, spanTags, spanLogs
    ) where

import Data.Int (Int32, Int64)
import GHC.Generics (Generic)

import Data.Set (Set)

import Data.Text (Text)

import Control.DeepSeq (NFData)

import Control.Lens (
    Lens', (&), (^.), (.~), _Empty, _Unwrapped, _Wrapped,
    from, lens, makeLenses, non, non')

import Pinch (Field, Pinchable, field, putField)

import Jaeger.Types.Flag (Flag, flags)
import Jaeger.Types.Log (Log)
import Jaeger.Types.SpanId (SpanId)
import Jaeger.Types.SpanRef (SpanRef)
import Jaeger.Types.Tag (Tag)
import Jaeger.Types.TimeSpec (TimeSpec, _TimeSpecUS)
import Jaeger.Types.TraceId (TraceId, traceId, traceIdHigh, traceIdLow)

spanId0 :: SpanId
spanId0 = 0 ^. _Unwrapped

-- | 'Span' represents a named unit of work performed by a service.
--
-- > struct Span {
-- >   1:  required i64           traceIdLow
-- >   2:  required i64           traceIdHigh
-- >   3:  required i64           spanId
-- >   4:  required i64           parentSpanId
-- >   5:  required string        operationName
-- >   6:  optional list<SpanRef> references
-- >   7:  required i32           flags
-- >   8:  required i64           startTime
-- >   9:  required i64           duration
-- >   10: optional list<Tag>     tags
-- >   11: optional list<Log>     logs
-- > }
data Span = Span { _spanTraceIdLow' :: !(Field 1 Int64)
                 , _spanTraceIdHigh' :: !(Field 2 Int64)
                 , _spanSpanId' :: !(Field 3 Int64)
                 , _spanParentSpanId' :: !(Field 4 Int64)
                 , _spanOperationName' :: !(Field 5 Text)
                 , _spanReferences' :: !(Field 6 (Maybe [SpanRef]))
                 , _spanFlags' :: !(Field 7 Int32)
                 , _spanStartTime' :: !(Field 8 Int64)
                 , _spanDuration' :: !(Field 9 Int64)
                 , _spanTags' :: !(Field 10 (Maybe [Tag]))
                 , _spanLogs' :: !(Field 11 (Maybe [Log]))
                 }
    deriving (Show, Eq, Generic)

makeLenses ''Span
instance NFData Span
instance Pinchable Span

-- | Construct a 'Span'.
--
-- /Note:/ The given 'TimeSpec' values are rounded down to microsecond
-- precision. See 'spanStartTime' and 'spanDuration'.
span :: TraceId  -- ^ 'spanTraceId'
     -> SpanId  -- ^ 'spanSpanId'
     -> Maybe SpanId  -- ^ 'spanParentSpanId'
     -> Text  -- ^ 'spanOperationName'
     -> [SpanRef]  -- ^ 'spanReferences'
     -> Set Flag  -- ^ 'spanFlags'
     -> TimeSpec  -- ^ 'spanStartTime'
     -> TimeSpec  -- ^ 'spanDuration'
     -> [Tag]  -- ^ 'spanTags'
     -> [Log]  -- ^ 'spanLogs'
     -> Span
span a b c d e f g h i j = span' a b c d & spanReferences .~ e
                                         & spanFlags .~ f
                                         & spanStartTime .~ g
                                         & spanDuration .~ h
                                         & spanTags .~ i
                                         & spanLogs .~ j
{-# INLINE span #-}

-- | A simplified version of 'span' which only takes required values.
--
-- Other fields are initialized to @0@, @[]@ or whatever is applicable and can
-- be set using the related lenses.
span' :: TraceId  -- ^ 'spanTraceId'
      -> SpanId  -- ^ 'spanSpanId'
      -> Maybe SpanId  -- ^ 'spanParentSpanId'
      -> Text  -- ^ 'spanOperationName'
      -> Span
span' a b c d = Span { _spanTraceIdLow' = putField (a ^. traceIdLow)
                     , _spanTraceIdHigh' = putField (a ^. traceIdHigh)
                     , _spanSpanId' = putField (b ^. _Wrapped)
                     , _spanParentSpanId' = putField (c ^. non spanId0 . _Wrapped)
                     , _spanOperationName' = putField d
                     , _spanReferences' = putField Nothing
                     , _spanFlags' = putField 0
                     , _spanStartTime' = putField 0
                     , _spanDuration' = putField 0
                     , _spanTags' = putField Nothing
                     , _spanLogs' = putField Nothing
                     }
{-# INLINE span' #-}

-- | 'Span' trace ID.
spanTraceId :: Lens' Span TraceId
spanTraceId = lens get set
  where
    get s = traceId (s ^. spanTraceIdLow' . field)
                    (s ^. spanTraceIdHigh' . field)
    set s t = s & spanTraceIdLow' . field .~ t ^. traceIdLow
                & spanTraceIdHigh' . field .~ t ^. traceIdHigh
{-# INLINE spanTraceId #-}

-- | 'Span' @spanId@.
--
-- Unique span ID (only unique within a given trace).
spanSpanId :: Lens' Span SpanId
spanSpanId = spanSpanId' . field . _Unwrapped
{-# INLINE spanSpanId #-}

-- | 'Span' @parentSpanId@.
--
-- Since nearly all spans will have parents spans, 'childOf' refs do not have
-- to be explicit.
spanParentSpanId :: Lens' Span (Maybe SpanId)
spanParentSpanId = spanParentSpanId' . field . _Unwrapped . from (non spanId0)
{-# INLINE spanParentSpanId #-}

-- | 'Span' @operationName@.
spanOperationName :: Lens' Span Text
spanOperationName = spanOperationName' . field
{-# INLINE spanOperationName #-}

-- | 'Span' @references@.
--
-- Causal references to other spans.
--
-- /Note:/ This is an 'optional' value in the Thrift message, which is mapped
-- transparantly from/to an empty list.
spanReferences :: Lens' Span [SpanRef]
spanReferences = spanReferences' . field . non' _Empty
{-# INLINE spanReferences #-}

-- | 'Span' @flags@.
--
-- /Note:/ This is an 'int32' bitfield value in the Thrift message, which is
-- mapped transparantly from/to a 'Set' of 'Flag's.
spanFlags :: Lens' Span (Set Flag)
spanFlags = spanFlags' . field . flags
{-# INLINE spanFlags #-}

-- | 'Span' @startTime@.
--
-- /Note:/ This is an unlawful 'Lens' because it breaks the first law, "You get
-- back what you put in": the precision of a provided 'TimeSpec' is dropped to
-- microseconds, so upon retrieval a different value can be returned.
spanStartTime :: Lens' Span TimeSpec
spanStartTime = spanStartTime' . field . _TimeSpecUS
{-# INLINE spanStartTime #-}

-- | 'Span' @duration@.
--
-- /Note:/ This is an unlawful 'Lens' because it breaks the first law, "You get
-- back what you put in": the precision of a provided 'TimeSpec' is dropped to
-- microseconds, so upon retrieval a different value can be returned.
spanDuration :: Lens' Span TimeSpec
spanDuration = spanDuration' . field . _TimeSpecUS
{-# INLINE spanDuration #-}

-- | 'Span' @tags@.
--
-- /Note:/ This is an 'optional' value in the Thrift message, which is mapped
-- transparantly from/to an empty list.
spanTags :: Lens' Span [Tag]
spanTags = spanTags' . field . non' _Empty
{-# INLINE spanTags #-}

-- | 'Span' @logs@.
--
-- /Note:/ This is an 'optional' value in the Thrift message, which is mapped
-- transparantly from/to an empty list.
spanLogs :: Lens' Span [Log]
spanLogs = spanLogs' . field . non' _Empty
{-# INLINE spanLogs #-}
