-- |
-- Module:      Jaeger.Types
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
--
-- Client code for the <https://uber.github.io/jaeger/ Jaeger>
-- <https://thrift.apache.org/ Thrift> interface.
--
-- This code is based on the Thrift IDL at
-- <https://github.com/uber/jaeger-idl/blob/cff5057c0174133a672472c92cab85e4ed2abac7/thrift/jaeger.thrift>
-- and <https://github.com/uber/jaeger-idl/blob/cff5057c0174133a672472c92cab85e4ed2abac7/thrift/agent.thrift>,
-- using the "Pinch" library for encoding/decoding.
--
-- It uses 'Control.Lens' for record field accessors, constructing values using
-- 'Prism's, 'Iso's using 'Wrapped',...

module Jaeger.Types (
    -- * RPC
      emitBatch
    -- ** Re-exports from "Pinch"
    , encodeMessage
    , binaryProtocol
    , compactProtocol

    -- * Batch
    , Batch
    -- ** Constructor function
    , batch
    -- ** Record field lenses
    , batchProcess, batchSpans
    -- ** Process
    , Process, process, processServiceName, processTags

    -- ** Span
    , Span
    -- *** Constructor function
    , Jaeger.Types.Span.span, span'
    -- *** Record field lenses
    , spanTraceId, spanSpanId, spanParentSpanId, spanOperationName
        , spanReferences, spanFlags, spanStartTime, spanDuration, spanTags, spanLogs
    -- *** Wrappers
    , TraceId, traceId, traceIdLow, traceIdHigh
    , SpanId
    , Flag, sampled, debug
    -- *** Re-exports from "clock"
    , TimeSpec

    -- *** Log
    , Log
    -- **** Constructor function
    , Jaeger.Types.Log.log
    -- **** Record field lenses
    , logTimestamp, logFields

    -- *** SpanRef
    , SpanRef
    -- **** Constructor function
    , spanRef
    -- **** Record field lenses
    , spanRefType, spanRefTraceId, spanRefSpanId
    -- **** SpanRefType
    , SpanRefType, childOf, followsFrom

    -- *** Tag
    , Tag
    -- **** Constructor functions
    , stringTag, doubleTag, boolTag, longTag, binaryTag
    -- **** Record field lenses
    , tagKey, tagType
    -- **** Prisms
    , _StringTag, _DoubleTag, _BoolTag, _LongTag, _BinaryTag
    -- **** Utilities
    , mapTagValue
    -- **** TagType
    , TagType, string, double, bool, long, binary

    -- * Carrier
    , Carrier(..)
    -- ** SpanContext
    , HasSpanContext(..)
    , SpanContext
    , spanContext
    ) where

import Pinch (binaryProtocol, compactProtocol, encodeMessage)

import Jaeger.Types.Batch (
      Batch
    , batch
    , batchProcess, batchSpans
    )

import Jaeger.Types.Flag (
      Flag
    , debug, sampled
    )

import Jaeger.Types.Log (
      Log
    , log
    , logTimestamp, logFields
    )

import Jaeger.Types.Process (
      Process
    , process
    , processServiceName
    , processTags
    )

import Jaeger.Types.RPC (
      emitBatch
    )

import Jaeger.Types.Span (
      Span
    , span, span'
    , spanTraceId, spanSpanId, spanParentSpanId, spanOperationName
        , spanReferences, spanFlags, spanStartTime, spanDuration, spanTags, spanLogs
    )

import Jaeger.Types.SpanContext(
      Carrier(extract, inject)
    , HasSpanContext(spanContextFlags, spanContextParentSpanId,
        spanContextSpanId, spanContextTraceId)
    , SpanContext
    , spanContext
    )

import Jaeger.Types.SpanId (
      SpanId
    )

import Jaeger.Types.SpanRef (
      SpanRef
    , spanRef
    , spanRefType, spanRefTraceId, spanRefSpanId
    , SpanRefType, childOf, followsFrom
    )

import Jaeger.Types.Tag (
      Tag
    , stringTag, doubleTag, boolTag, longTag, binaryTag
    , tagKey, tagType
    , _StringTag, _DoubleTag, _BoolTag, _LongTag, _BinaryTag
    , mapTagValue
    , TagType, string, double, bool, long, binary
    )

import Jaeger.Types.TimeSpec (
      TimeSpec
    )

import Jaeger.Types.TraceId (
      TraceId
    , traceId
    , traceIdHigh, traceIdLow
    )
