{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Jaeger.Types.SpanContext (
      HasSpanContext(..)
    , SpanContext
    , spanContext
    , Carrier(..)
    ) where

import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Word (Word8, Word64)
import GHC.Generics (Generic)

import Data.Set (Set)

import Control.Lens (
    Getter,
    (&), (<&>), (^.), (%~), _1, _Right, _Unwrapped, _Wrapped,
    from, non, re, to, view)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSB

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Read as TLR
import qualified Data.Text.Read as TR

import Formatting (Format, (%), (%.), bprint, format, hex, later, left, mapf, sformat)

import Data.Binary.Get (getInt32be, getWord8, getWord64be, runGetOrFail)

import Network.HTTP.Types.Header (Header, HeaderName)

import Jaeger.Types.Flag (Flag, flags)
import Jaeger.Types.Span (Span, spanFlags, spanParentSpanId, spanSpanId, spanTraceId)
import Jaeger.Types.SpanId (SpanId)
import Jaeger.Types.TraceId (TraceId, traceId, traceIdHigh, traceIdLow)

-- | Class for types that can fully reference a 'Span'.
class HasSpanContext s where
    -- | Referred span 'TraceId'.
    spanContextTraceId :: Getter s TraceId
    -- | Referred span 'SpanId'.
    spanContextSpanId :: Getter s SpanId
    -- | Referred span parent 'SpanId', if any. Span @0@ is 'Nothing'.
    spanContextParentSpanId :: Getter s (Maybe SpanId)
    -- | Referred span 'Flag's.
    spanContextFlags :: Getter s (Set Flag)

instance HasSpanContext Span where
    spanContextTraceId = spanTraceId
    spanContextSpanId = spanSpanId
    spanContextParentSpanId = spanParentSpanId
    spanContextFlags = spanFlags

-- | Minimal reference to a 'Span'.
data SpanContext = SpanContext !TraceId !SpanId !(Maybe SpanId) !(Set Flag)
    deriving (Show, Eq, Generic)

-- | Construct a 'SpanContext'.
--
-- A parent 'SpanId' of @0@ gets mapped to 'Nothing' automatically.
--
-- /Note:/ Mosty exported for testing purposes, and to implement custom
-- 'Carrier's.
spanContext :: TraceId  -- ^ Referred span 'TraceId'
            -> SpanId  -- ^ Referreed span 'SpanId'
            -> SpanId  -- ^ Referred span parent 'SpanId'
            -> Set Flag  -- ^ Referred span 'Flag's
            -> SpanContext
spanContext tid sid psid = SpanContext tid sid (if psid == span0 then Nothing else Just psid)
  where
    span0 = 0 ^. _Unwrapped

instance HasSpanContext SpanContext where
    spanContextTraceId = to (\(SpanContext tid _ _ _) -> tid)
    spanContextSpanId = to (\(SpanContext _ sid _ _) -> sid)
    spanContextParentSpanId = to (\(SpanContext _ _ psid _) -> psid)
    spanContextFlags = to (\(SpanContext _ _ _ f) -> f)


-- | A 'Carrier' is a type to which a 'HasSpanContext' can be serialized, or a 'SpanContext' extracted from.
class Carrier c where
    -- | Serialize a 'HasSpanContext'.
    inject :: HasSpanContext s => s -> c
    -- | Extract a 'SpanContext', or error out.
    extract :: c -> Either String SpanContext

-- | A sort-of identity 'Carrier'.
instance Carrier SpanContext where
    inject s = SpanContext (s ^. spanContextTraceId)
                           (s ^. spanContextSpanId)
                           (s ^. spanContextParentSpanId)
                           (s ^. spanContextFlags)
    extract = Right


formatSpanContext :: HasSpanContext s => Format r (s -> r)
formatSpanContext = spanContextTraceId' % ":" <> spanContextSpanId' % ":" <> spanContextParentSpanId' % ":" <> spanContextFlags'
  where
    spanContextTraceId' = later (\t ->
        let tid = t ^. spanContextTraceId in
        let low = tid ^. traceIdLow . to fromIntegral in
        case tid ^. traceIdHigh . to fromIntegral of
            0 -> bprint hex64 low
            high -> bprint (hex64 % (left 16 '0' %. hex64)) high low)
    spanContextSpanId' = mapf (view (spanContextSpanId . _Wrapped . to fromIntegral)) hex64
    spanContextParentSpanId' = mapf (view (spanContextParentSpanId . non (0 ^. _Unwrapped) . _Wrapped . to fromIntegral)) hex64
    spanContextFlags' = mapf (view (spanContextFlags . from flags)) hex8
    hex64 = hex :: Format r (Word64 -> r)
    hex8 = hex :: Format r (Word8 -> r)

-- | A 'Builder' 'Carrier'.
--
-- Likely only 'inject' is ever useful.
--
-- /Note:/ For 'extract', the 'Builder' is first evaluated into a
-- 'Data.Text.Lazy.Text' value before being parsed.
instance Carrier TLB.Builder where
    inject = bprint formatSpanContext
    extract = extract . TLB.toLazyTextWith (16 + 16 + 1 + 16 + 1 + 16 + 1 + 2)

-- | Human-readable 'T.Text' 'Carrier'.
--
-- This formatting is used in the HTTP 'Header' instance.
instance Carrier T.Text where
    inject = sformat formatSpanContext

    extract t | T.null t = Left "Empty string"
              | otherwise = case T.splitOn ":" t of
                  [tid, sid, psid, f] -> spanContext <$> parseTraceId tid
                                                     <*> parseSpanId sid
                                                     <*> parseSpanId psid
                                                     <*> parseFlags f
                  _ -> Left "Incorrect number of fields"
      where
        parseTraceId t' = if T.length t' > 16
            then let (high, low) = T.splitAt (T.length t' - 16) t' in
                 traceId <$> i64 low <*> i64 high
            else flip traceId 0 <$> i64 t'
        i64 t' = TR.hexadecimal t' & _Right %~ view (_1 . to (fromIntegral @Word64 @Int64))
        parseSpanId :: T.Text -> Either String SpanId
        parseSpanId t' = TR.hexadecimal t' & _Right %~ view (_1 . _Unwrapped)
        parseFlags t' = TR.hexadecimal t' & _Right %~ view (_1 . flags @Word8)

-- | Human-readable 'TL.Text' 'Carrier'.
--
-- Similar to the 'T.Text' instance.
instance Carrier TL.Text where
    inject = format formatSpanContext

    extract t | TL.null t = Left "Empty string"
              | otherwise = case TL.splitOn ":" t of
                  [tid, sid, psid, f] -> spanContext <$> parseTraceId tid
                                                     <*> parseSpanId sid
                                                     <*> parseSpanId psid
                                                     <*> parseFlags f
                  _ -> Left "Incorrect number of fields"
      where
        parseTraceId t' = if TL.length t' > 16
            then let (high, low) = TL.splitAt (TL.length t' - 16) t' in
                 traceId <$> i64 low <*> i64 high
            else flip traceId 0 <$> i64 t'
        i64 t' = TLR.hexadecimal t' & _Right %~ view (_1 . to (fromIntegral @Word64 @Int64))
        parseSpanId :: TL.Text -> Either String SpanId
        parseSpanId t' = TLR.hexadecimal t' & _Right %~ view (_1 . _Unwrapped)
        parseFlags t' = TLR.hexadecimal t' & _Right %~ view (_1 . flags @Word8)


-- | Binary blob 'Carrier'.
instance Carrier BSL.ByteString where
    inject a = let b = BSB.word64BE (a ^. spanContextTraceId . traceIdHigh . to fromIntegral)
                    <> BSB.word64BE (a ^. spanContextTraceId . traceIdLow . to fromIntegral)
                    <> BSB.word64BE (a ^. spanContextSpanId . _Wrapped . to fromIntegral)
                    <> BSB.word64BE (a ^. spanContextParentSpanId . non (0 ^. _Unwrapped) . _Wrapped . to fromIntegral)
                    <> BSB.word8 (a ^. spanContextFlags . re flags)
                    <> BSB.int32BE 0
               in BSB.toLazyByteString b

    extract s = case runGetOrFail parse s of
        Left (_, _, m) -> Left m
        Right (_, _, a) -> Right a
      where
        parse = spanContext <$> (flip traceId <$> (getWord64be <&> fromIntegral)
                                              <*> (getWord64be <&> fromIntegral))
                            <*> (getWord64be <&> view (to fromIntegral . _Unwrapped))
                            <*> (getWord64be <&> view (to fromIntegral . _Unwrapped))
                            <*> (getWord8 <&> view flags)
                            <*  getInt32be

-- | Binary blob 'Carrier'.
--
-- Similar to the 'BSL.ByteString' instance.
instance Carrier BS.ByteString where
    inject = BSL.toStrict . inject
    extract = extract . BSL.fromStrict


headerName :: HeaderName
headerName = "uber-trace-id"

-- | HTTP 'Header' 'Carrier'.
instance Carrier [Header] where
    inject s = [(headerName, T.encodeUtf8 $ inject s)]
    extract c = case lookup headerName c of
        Nothing -> Left $ "No " <> show headerName <> " header"
        Just t -> extract $ T.decodeUtf8 t

-- | HTTP header 'Carrier' for 'T.Text' values.
instance Carrier [(HeaderName, T.Text)] where
    inject s = [(headerName, inject s)]
    extract c = case lookup headerName c of
        Nothing -> Left $ "No " <> show headerName <> " header"
        Just t -> extract t

-- | HTTP header 'Carrier' for 'TL.Text' values.
instance Carrier [(HeaderName, TL.Text)] where
    inject s = [(headerName, inject s)]
    extract c = case lookup headerName c of
        Nothing -> Left $ "No " <> show headerName <> " header"
        Just t -> extract t
