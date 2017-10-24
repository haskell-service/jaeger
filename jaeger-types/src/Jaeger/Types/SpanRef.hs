{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jaeger.Types.SpanRef (
      SpanRef
    , spanRef
    , spanRefType, spanRefTraceId, spanRefSpanId
    , SpanRefType, childOf, followsFrom
    ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Control.Lens (Lens', (&), (^.), (.~), _Unwrapped, _Wrapped, lens, makeLenses)

import Pinch (Enumeration, Field, Pinchable, enum, field, putField)

import Jaeger.Types.SpanId (SpanId)
import Jaeger.Types.TraceId (TraceId, traceId, traceIdHigh, traceIdLow)

-- | Type of a 'SpanRef'.
--
-- > enum SpanRefType { CHILD_OF, FOLLOWS_FROM }
data SpanRefType = CHILD_OF (Enumeration 0)
                 | FOLLOWS_FROM (Enumeration 1)
    deriving (Show, Eq, Generic)

instance NFData SpanRefType
instance Pinchable SpanRefType

-- | 'child-of' relation.
childOf :: SpanRefType
childOf = CHILD_OF enum

-- | 'follows-from' relation.
followsFrom :: SpanRefType
followsFrom = FOLLOWS_FROM enum


-- | 'SpanRef' describes causal relationship of the current 'Span' to another 'Span' (e.g. 'childOf').
--
-- > struct SpanRef {
-- >   1: required SpanRefType refType
-- >   2: required i64         traceIdLow
-- >   3: required i64         traceIdHigh
-- >   4: required i64         spanId
-- > }
data SpanRef = SpanRef { _spanRefRefType' :: !(Field 1 SpanRefType)
                       , _spanRefTraceIdLow' :: !(Field 2 Int64)
                       , _spanRefTraceIdHigh' :: !(Field 3 Int64)
                       , _spanRefSpanId' :: !(Field 4 Int64)
                       }
    deriving (Show, Eq,  Generic)

makeLenses ''SpanRef
instance NFData SpanRef
instance Pinchable SpanRef

-- | Construct a 'SpanRef'.
spanRef :: SpanRefType -> TraceId -> SpanId -> SpanRef
spanRef a b c = SpanRef { _spanRefRefType' = putField a
                        , _spanRefTraceIdLow' = putField (b ^. traceIdLow)
                        , _spanRefTraceIdHigh' = putField (b ^. traceIdHigh)
                        , _spanRefSpanId' = putField (c ^. _Wrapped)
                        }
{-# INLINE spanRef #-}

-- | 'SpanRef' @refType@.
spanRefType :: Lens' SpanRef SpanRefType
spanRefType = spanRefRefType' . field
{-# INLINE spanRefType #-}

-- | 'SpanRef' trace ID.
spanRefTraceId :: Lens' SpanRef TraceId
spanRefTraceId = lens get set
  where
    get s = traceId (s ^. spanRefTraceIdLow' . field)
                    (s ^. spanRefTraceIdHigh' . field)
    set s t = s & spanRefTraceIdLow' . field .~ t ^. traceIdLow
                & spanRefTraceIdHigh' . field .~ t ^. traceIdHigh
{-# INLINE spanRefTraceId #-}

-- | 'SpanRef' @spanId@.
spanRefSpanId :: Lens' SpanRef SpanId
spanRefSpanId = spanRefSpanId' . field . _Unwrapped
{-# INLINE spanRefSpanId #-}
