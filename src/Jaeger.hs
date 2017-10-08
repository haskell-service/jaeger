{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Module:      Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: BangPatterns, DataKinds, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies
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

module Jaeger (
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
    , span, span'
    -- *** Record field lenses
    , spanTraceId, spanSpanId, spanParentSpanId, spanOperationName
        , spanReferences, spanFlags, spanStartTime, spanDuration, spanTags, spanLogs
    -- *** Utilities
    , timeSpan
    , timeSpan'
    -- *** Wrappers
    , TraceId, traceId, traceIdLow, traceIdHigh
    , SpanId, span0
    , SpanFlag, sampled, debug
    -- *** Re-exports from "clock"
    , TimeSpec

    -- *** Log
    , Log
    -- **** Constructor function
    , log
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
    ) where

import Prelude hiding (log, span)

import Data.Bits (setBit, testBit, zeroBits)
import Data.Int (Int32, Int64)

import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import System.Random (Random(randomR, random))

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString (ByteString)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Control.Lens hiding ((.=), enum, set)

import System.Clock (
    Clock(Boottime, Realtime), TimeSpec,
    diffTimeSpec, fromNanoSecs, getTime, toNanoSecs)

import Pinch (
    Enumeration, Field, Message, MessageType(Oneway), Pinchable,
    (.=), binaryProtocol, compactProtocol, encodeMessage, enum, field,
    mkMessage, putField, struct)


-- | 'TagType' denotes the type of a 'Tag's value.
--
-- > enum TagType { STRING, DOUBLE, BOOL, LONG, BINARY }
data TagType = STRING (Enumeration 0)
             | DOUBLE (Enumeration 1)
             | BOOL (Enumeration 2)
             | LONG (Enumeration 3)
             | BINARY (Enumeration 4)
    deriving (Show, Eq, Generic)

instance Pinchable TagType

-- | 'Tag' carries a 'Text' value.
string :: TagType
string = STRING enum
-- | 'Tag' carries a 'Double' value.
double :: TagType
double = DOUBLE enum
-- | 'Tag' carries a 'Bool' value.
bool :: TagType
bool = BOOL enum
-- | 'Tag' carries an 'Int64' value.
long :: TagType
long = LONG enum
-- | 'Tag' carries a 'ByteString' value.
binary :: TagType
binary = BINARY enum


-- | 'Tag' is a basic strongly typed key/value pair.
--
-- > struct Tag {
-- >   1: required string  key
-- >   2: required TagType vType
-- >   3: optional string  vStr
-- >   4: optional double  vDouble
-- >   5: optional bool    vBool
-- >   6: optional i64     vLong
-- >   7: optional binary  vBinary
-- > }
data Tag = Tag { _tagKey' :: !(Field 1 Text)
               , _tagVType' :: !(Field 2 TagType)
               , _tagVStr' :: !(Field 3 (Maybe Text))
               , _tagVDouble' :: !(Field 4 (Maybe Double))
               , _tagVBool' :: !(Field 5 (Maybe Bool))
               , _tagVLong':: !(Field 6 (Maybe Int64))
               , _tagVBinary' :: !(Field 7 (Maybe ByteString))
               }
    deriving (Show, Eq, Generic)

makeLenses ''Tag
instance Pinchable Tag

-- | 'Tag' @key@.
tagKey :: Lens' Tag Text
tagKey = tagKey' . field
{-# INLINE tagKey #-}

-- | 'Tag' @vType@.
--
-- /Note:/ `tagType` is 'Getter', /not/ a 'Lens', otherwise it'd be too easy
-- to create invalid 'Tag's where the field corresponding with the 'TagType'
-- isn't set.
tagType :: Getter Tag TagType
tagType = tagVType' . field
{-# INLINE tagType #-}

fromTagUnion :: HasCallStack => Getting a (Maybe a) a
fromTagUnion f = maybe (error "Tag: mismatch between vType and optional fields") (fmap Just . f)
{-# INLINE fromTagUnion #-}

-- | Map a family of functions over the value carried in a 'Tag'.
mapTagValue :: (Text -> a)  -- ^ Handle a 'Text' payload
            -> (Double -> a)  -- ^ Handle a 'Double' payload
            -> (Bool -> a)  -- ^ Handle a 'Bool' payload
            -> (Int64 -> a)  -- ^ Handle an 'Int64' payload
            -> (ByteString -> a)  -- ^ Handle a 'ByteStrin' payload
            -> Tag  -- ^ 'Tag' whose value to map over
            -> a
mapTagValue t d b i b' tag = case tag ^. tagVType' . field of
    STRING _ -> t (tag ^. tagVStr' . field . fromTagUnion)
    DOUBLE _ -> d (tag ^. tagVDouble' . field . fromTagUnion)
    BOOL _ -> b (tag ^. tagVBool' . field . fromTagUnion)
    LONG _ -> i (tag ^. tagVLong' . field . fromTagUnion)
    BINARY _ -> b' (tag ^. tagVBinary' . field . fromTagUnion)

-- Note: This is a placeholder, but actually an invalid Tag.
emptyTag :: Tag
emptyTag = Tag { _tagKey' = putField Text.empty
               , _tagVType' = putField string
               , _tagVStr' = putField Nothing
               , _tagVDouble' = putField Nothing
               , _tagVBool' = putField Nothing
               , _tagVLong' = putField Nothing
               , _tagVBinary' = putField Nothing
               }
{-# INLINE emptyTag #-}

-- | 'Prism' for a 'Tag' carrying 'Text'.
_StringTag :: Prism' Tag (Text, Text)
_StringTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ string
                           & tagVStr' . field ?~ v
    extract t = case t ^. tagVType' . field of
        STRING _ -> Right (t ^. tagKey, t ^. tagVStr' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying a 'Double'.
_DoubleTag :: Prism' Tag (Text, Double)
_DoubleTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ double
                           & tagVDouble' . field ?~ v
    extract t = case t ^. tagVType' . field of
        DOUBLE _ -> Right (t ^. tagKey, t ^. tagVDouble' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying a 'Bool'.
_BoolTag :: Prism' Tag (Text, Bool)
_BoolTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ bool
                           & tagVBool' . field ?~ v
    extract t = case t ^. tagVType' . field of
        BOOL _ -> Right (t ^. tagKey, t ^. tagVBool' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying an 'Int64'.
_LongTag :: Prism' Tag (Text, Int64)
_LongTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ long
                           & tagVLong' . field ?~ v
    extract t = case t ^. tagVType' . field of
        LONG _ -> Right (t ^. tagKey, t ^. tagVLong' . field . fromTagUnion)
        _ -> Left t

-- | 'Prism' for a 'Tag' carrying a 'ByteString'.
_BinaryTag :: Prism' Tag (Text, ByteString)
_BinaryTag = prism wrap extract
  where
    wrap (k, v) = emptyTag & tagKey .~ k
                           & tagVType' . field .~ binary
                           & tagVBinary' . field ?~ v
    extract t = case t ^. tagVType' . field of
        BINARY _ -> Right (t ^. tagKey, t ^. tagVBinary' . field . fromTagUnion)
        _ -> Left t


-- | Construct a 'Text' 'Tag'.
stringTag :: Text -> Text -> Tag
stringTag = curry (review _StringTag)
{-# INLINE stringTag #-}

-- | Construct a 'Double' 'Tag'.
doubleTag :: Text -> Double -> Tag
doubleTag = curry (review _DoubleTag)
{-# INLINE doubleTag #-}

-- | Construct a 'Bool' 'Tag'.
boolTag :: Text -> Bool -> Tag
boolTag = curry (review _BoolTag)
{-# INLINE boolTag #-}

-- | Construct an 'Int64' 'Tag'.
longTag :: Text -> Int64 -> Tag
longTag = curry (review _LongTag)
{-# INLINE longTag #-}

-- | Construct a 'ByteString' 'Tag'.
binaryTag :: Text -> ByteString -> Tag
binaryTag = curry (review _BinaryTag)
{-# INLINE binaryTag #-}


-- | Unlawful 'Iso' between an 'Int64' (in microseconds) and a 'TimeSpec'.
--
-- /Note:/ This is not a lawful 'Iso' because it drops precision, so the
-- "'view'/'review' equals identity" law is broken.
_TimeSpecUS :: Iso' Int64 TimeSpec
_TimeSpecUS = iso
    (fromNanoSecs . fromIntegral . (* 1000))
    ((`div` 1000) . fromInteger . toNanoSecs)
{-# INLINE _TimeSpecUS #-}


-- | 'Log' is a timed event with an arbitrary set of 'Tag's.
--
-- > struct Log {
-- >   1: required i64       timestamp
-- >   2: required list<Tag> fields
-- > }
data Log = Log { _logTimestamp' :: !(Field 1 Int64)
               , _logFields' :: !(Field 2 [Tag])
               }
    deriving (Show, Eq, Generic)

makeLenses ''Log
instance Pinchable Log

-- | Construct a 'Log'.
--
-- /Note:/ The given 'TimeSpec' is rounded down to microsecond precision. See
-- 'logTimestamp'.
log :: TimeSpec -> [Tag] -> Log
log a b = Log { _logTimestamp' = putField (a ^. re _TimeSpecUS)
              , _logFields' = putField b
              }
{-# INLINE log #-}

-- | 'Log' @timestamp@.
--
-- /Note:/ This is an unlawful 'Lens' because it breaks the first law, "You get
-- back what you put in": the precision of a provided 'TimeSpec' is dropped to
-- microseconds, so upon retrieval a different value can be returned.
logTimestamp :: Lens' Log TimeSpec
logTimestamp = logTimestamp' . field . _TimeSpecUS
{-# INLINE logTimestamp #-}

-- | 'Log' @fields@.
logFields :: Lens' Log [Tag]
logFields = logFields' . field
{-# INLINE logFields #-}


-- | Type of a 'SpanRef'.
--
-- > enum SpanRefType { CHILD_OF, FOLLOWS_FROM }
data SpanRefType = CHILD_OF (Enumeration 0)
                 | FOLLOWS_FROM (Enumeration 1)
    deriving (Show, Eq, Generic)

instance Pinchable SpanRefType

-- | 'child-of' relation.
childOf :: SpanRefType
childOf = CHILD_OF enum

-- | 'follows-from' relation.
followsFrom :: SpanRefType
followsFrom = FOLLOWS_FROM enum


-- | Representation of a trace ID.
data TraceId = TraceId { _traceIdLow :: !Int64
                       , _traceIdHigh :: !Int64
                       }
    deriving (Show, Eq, Ord, Generic)

makeLensesWith (lensRules & generateSignatures .~ False) ''TraceId

instance Random TraceId where
    random g =
        let (low, g') = random g in
        let (high, g'') = random g' in
        let t = TraceId { _traceIdLow = low
                        , _traceIdHigh = high
                        }
        in
        (t, g'')

    randomR (lo, hi) g =
        let (low, g') = randomR (_traceIdLow lo, _traceIdLow hi) g in
        let (high, g'') = randomR (_traceIdHigh lo, _traceIdHigh hi) g' in
        let t = TraceId { _traceIdLow = low
                        , _traceIdHigh = high
                        }
        in
        (t, g'')

-- | Construct a 'TraceId'.
traceId :: Int64 -> Int64 -> TraceId
traceId low high = TraceId { _traceIdLow = low
                           , _traceIdHigh = high
                           }
{-# INLINE traceId #-}

-- | The least significant 64 bits of a 'TraceId'.
traceIdLow :: Lens' TraceId Int64

-- | The most significant 64 bits of a 'TraceId'.
--
-- @0@ when only 64bit IDs are used.
traceIdHigh :: Lens' TraceId Int64


-- | Representation of a span ID.
newtype SpanId = SpanId Int64
    deriving (Show, Eq, Ord, Generic)

makeWrapped ''SpanId

instance Random SpanId where
    random g = let (a, g') = random g in (SpanId a, g')
    randomR (SpanId l, SpanId h) g = let (a, g') = randomR (l, h) g in (SpanId a, g')

-- | 'SpanId 0', the 'spanParentSpanId' of all 'Span's.
span0 :: SpanId
span0 = SpanId 0


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
    get s = TraceId { _traceIdLow = s ^. spanRefTraceIdLow' . field
                    , _traceIdHigh = s ^. spanRefTraceIdHigh' . field
                    }
    set s t = s & spanRefTraceIdLow' . field .~ t ^. traceIdLow
                & spanRefTraceIdHigh' . field .~ t ^. traceIdHigh
{-# INLINE spanRefTraceId #-}

-- | 'SpanRef' @spanId@.
spanRefSpanId :: Lens' SpanRef SpanId
spanRefSpanId = spanRefSpanId' . field . _Unwrapped
{-# INLINE spanRefSpanId #-}


-- | Representation of a 'Span' flag (see 'spanFlags').
data SpanFlag = Sampled | Debug
    deriving (Show, Eq, Enum, Bounded, Ord, Generic)

-- | A sampled 'Span'.
sampled :: SpanFlag
sampled = Sampled

-- | A debug 'Span'.
debug :: SpanFlag
debug = Debug


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
instance Pinchable Span

-- | Construct a 'Span'.
--
-- /Note:/ The given 'TimeSpec' values are rounded down to microsecond
-- precision. See 'spanStartTime' and 'spanDuration'.
span :: TraceId  -- ^ 'spanTraceId'
     -> SpanId  -- ^ 'spanSpanId'
     -> SpanId  -- ^ 'spanParentSpanId'
     -> Text  -- ^ 'spanOperationName'
     -> [SpanRef]  -- ^ 'spanReferences'
     -> Set SpanFlag  -- ^ 'spanFlags'
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
-- be set using the related lenses, or e.g. using 'timeSpan'.
span' :: TraceId  -- ^ 'spanTraceId'
      -> SpanId  -- ^ 'spanSpanId'
      -> SpanId  -- ^ 'spanParentSpanId'
      -> Text  -- ^ 'spanOperationName'
      -> Span
span' a b c d = Span { _spanTraceIdLow' = putField (a ^. traceIdLow)
                     , _spanTraceIdHigh' = putField (a ^. traceIdHigh)
                     , _spanSpanId' = putField (b ^. _Wrapped)
                     , _spanParentSpanId' = putField (c ^. _Wrapped)
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
    get s = TraceId { _traceIdLow = s ^. spanTraceIdLow' . field
                    , _traceIdHigh = s ^. spanTraceIdHigh' . field
                    }
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
spanParentSpanId :: Lens' Span SpanId
spanParentSpanId = spanParentSpanId' . field . _Unwrapped
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
-- mapped transparantly from/to a 'Set' of 'SpanFlag's.
spanFlags :: Lens' Span (Set SpanFlag)
spanFlags = spanFlags' . field . iso unwrap wrap
  where
    unwrap i = Set.filter (testBit i . bitPos) (Set.fromList [minBound..maxBound])
    wrap = foldr (\a b -> setBit b (bitPos a)) zeroBits
    bitPos f = case f of
        Sampled -> 0
        Debug -> 1
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


-- | Capture timing of a given action and propagate related 'Span' fields.
--
-- This utility function will run the given action, and measure the time it
-- takes to complete. It will then fill the 'spanStartTime' and 'spanDuration'
-- fields of a given 'Span' and return the updated 'Span', as well as the result
-- of the action.
timeSpan :: MonadIO m => m a -> Span -> m (a, Span)
timeSpan act s = do
    (startTime, begin) <- liftIO $ (,) <$> getTime Realtime <*> getTime Boottime
    a <- act
    end <- liftIO $ getTime Boottime
    return (a, s & spanStartTime .~ startTime
                 & spanDuration .~ diffTimeSpec end begin)

-- | Simplified version of 'timeSpan' for 'void' actions.
timeSpan' :: MonadIO m => m () -> Span -> m Span
timeSpan' act s = snd <$> timeSpan act s
{-# INLINE timeSpan' #-}


-- | 'Process' describes the traced process/service that emits 'Span's.
--
-- > struct Process {
-- >   1: required string    serviceName
-- >   2: optional list<Tag> tags
-- > }
data Process = Process { _processServiceName' :: !(Field 1 Text)
                       , _processTags' :: !(Field 2 (Maybe [Tag]))
                       }
    deriving (Show, Eq, Generic)

makeLenses ''Process
instance Pinchable Process

-- | Construct a 'Process'.
process :: Text -> [Tag] -> Process
process a b = Process { _processServiceName' = putField a
                      , _processTags' = putField $ if null b then Nothing else Just b
                      }
{-# INLINE process #-}

-- | 'Process' @serviceName@.
processServiceName :: Lens' Process Text
processServiceName = processServiceName' . field
{-# INLINE processServiceName #-}

-- | 'Process' @tags@.
--
-- /Note:/ This is an 'optional' value in the Thrift message, which is mapped
-- transparantly from/to an empty list.
processTags :: Lens' Process [Tag]
processTags = processTags' . field . non' _Empty
{-# INLINE processTags #-}


-- | 'Batch' is a collection of 'Span's reported out of 'Process'.
--
-- > struct Batch {
-- >   1: required Process    process
-- >   2: required list<Span> spans
-- > }
data Batch = Batch { _batchProcess' :: !(Field 1 Process)
                   , _batchSpans' :: !(Field 2 [Span])
                   }
    deriving (Show, Eq, Generic)

makeLenses ''Batch
instance Pinchable Batch

-- | Construct a 'Batch'.
batch :: Process -> [Span] -> Batch
batch a b = Batch { _batchProcess' = putField a
                  , _batchSpans' = putField b
                  }
{-# INLINE batch #-}

-- | 'Batch' @process@.
batchProcess :: Lens' Batch Process
batchProcess = batchProcess' . field
{-# INLINE batchProcess #-}

-- | 'Batch' @spans@.
batchSpans :: Lens' Batch [Span]
batchSpans = batchSpans' . field
{-# INLINE batchSpans #-}


-- | Construct an 'Agent.emitBatch' RPC request message.
--
-- > service Agent {
-- >     oneway void emitBatch(1: jaeger.Batch batch)
-- > }
emitBatch :: Batch  -- ^ Batch to emit
          -> Message
emitBatch !batch' = mkMessage (Text.pack "emitBatch") Oneway 0 $
    struct [ 1 .= batch' ]
{-# INLINE emitBatch #-}
