{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Control.Lens ((^.), _Unwrapped)

import Control.DeepSeq (NFData)

import Data.ByteString.Lazy (ByteString)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Pinch (encode)

import Weigh (func, mainWith, value)

import Thrift.Protocol (Protocol, serializeVal)
import Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import Thrift.Protocol.Compact (CompactProtocol(CompactProtocol))
import Thrift.Transport (Transport)
import Thrift.Transport.Empty (EmptyTransport(EmptyTransport))
import Thrift.Types (ThriftType(T_STRUCT), ThriftVal(TList), TypeMap)

import Jaeger.Types (
    SpanId, binaryProtocol, compactProtocol, emitBatch, encodeMessage, traceId)

import Values (batch, process, spans, tags)
import qualified ThriftValues as T
import qualified Jaeger_Types as T

instance NFData T.SpanRefType
instance NFData T.Log
instance NFData T.TagType
instance NFData T.SpanRef
instance NFData T.Tag
instance NFData T.Process
instance NFData T.Span
instance NFData T.Batch

main :: IO ()
main = mainWith $ do
    value "value/SpanId 0" (0 ^. _Unwrapped :: SpanId)
    value "value/traceId 0 0" (traceId 0 0)

    value "value/Pinch/tags" tags
    value "value/Thrift/tags" T.tags
    value "value/Pinch/process" process
    value "value/Thrift/process" T.process
    value "value/Pinch/spans" spans
    value "value/Thrift/spans" T.spans
    value "value/Pinch/batch" batch
    value "value/Thrift/batch" T.batch

    func "emitBatch" emitBatch batch

    func "encode/binaryProtocol/Pinch/tags" (encode binaryProtocol) tags
    func "encode/binaryProtocol/Thrift/tags" (doVector T.from_Tag T.typemap_Tag (BinaryProtocol EmptyTransport)) T.tags
    func "encode/binaryProtocol/Pinch/process" (encode binaryProtocol) process
    func "encode/binaryProtocol/Thrift/process" (T.encode_Process (BinaryProtocol EmptyTransport)) T.process
    func "encode/binaryProtocol/Pinch/spans" (encode binaryProtocol) spans
    func "encode/binaryProtocol/Thrift/spans" (doVector T.from_Span T.typemap_Span (BinaryProtocol EmptyTransport)) T.spans
    func "encode/binaryProtocol/Pinch/batch" (encode binaryProtocol) batch
    func "encode/binaryProtocol/Thrift/batch" (T.encode_Batch (BinaryProtocol EmptyTransport)) T.batch

    func "encode/compactProtocol/Pinch/tags" (encode compactProtocol) tags
    func "encode/compactProtocol/Thrift/tags" (doVector T.from_Tag T.typemap_Tag (CompactProtocol EmptyTransport)) T.tags
    func "encode/compactProtocol/Pinch/process" (encode compactProtocol) process
    func "encode/compactProtocol/Thrift/process" (T.encode_Process (CompactProtocol EmptyTransport)) T.process
    func "encode/compactProtocol/Pinch/spans" (encode compactProtocol) spans
    func "encode/compactProtocol/Thrift/spans" (doVector T.from_Span T.typemap_Span (CompactProtocol EmptyTransport)) T.spans
    func "encode/compactProtocol/Pinch/batch" (encode compactProtocol) batch
    func "encode/compactProtocol/Thrift/batch" (T.encode_Batch (CompactProtocol EmptyTransport)) T.batch

    func "encodeMessage/binaryProtocol" (encodeMessage binaryProtocol . emitBatch) batch
    func "encodeMessage/compactProtocol" (encodeMessage compactProtocol . emitBatch) batch

doVector :: (Protocol p, Transport t)
         => (a -> ThriftVal)
         -> TypeMap
         -> p t
         -> Vector a
         -> ByteString
doVector from typemap pt vb = serializeVal pt (TList (T_STRUCT typemap) (map from $ Vector.toList vb))
