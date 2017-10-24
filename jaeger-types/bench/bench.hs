{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Data.ByteString.Lazy (ByteString)

import Pinch (Pinchable, binaryProtocol, compactProtocol, encode, encodeMessage)

import Criterion (Benchmark, bench, bgroup, nf)
import Criterion.Main (defaultMain)

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Jaeger.Types (emitBatch)

import Values (batch, process, spans, tags)
import qualified ThriftValues as T
import qualified Jaeger_Types as T

import Thrift.Protocol (Protocol, serializeVal)
import Thrift.Protocol.Binary (BinaryProtocol(BinaryProtocol))
import Thrift.Protocol.Compact (CompactProtocol(CompactProtocol))
import Thrift.Transport (Transport)
import Thrift.Transport.Empty (EmptyTransport(EmptyTransport))
import Thrift.Types (ThriftType(T_STRUCT), ThriftVal(TList), TypeMap)

main :: IO ()
main = defaultMain [
    bench "emitBatch" $ nf emitBatch batch
  , bgroup "encode" [
        bgroup "tags" $ encodeBench tags (doVector T.from_Tag T.typemap_Tag) T.tags
      , bgroup "spans" $ encodeBench spans (doVector T.from_Span T.typemap_Span) T.spans
      , bgroup "process" $ encodeBench process T.encode_Process T.process
      , bgroup "batch" $ encodeBench batch T.encode_Batch T.batch
      ]
  , bgroup "encodeMessage" [
        bench "binaryProtocol" $ nf (encodeMessage binaryProtocol . emitBatch) batch
      , bench "compactProtocol" $ nf (encodeMessage compactProtocol . emitBatch) batch
      ]
  ]
  where
    encodeBench :: Pinchable a
                => a
                -> (forall p t. (Protocol p, Transport t) => p t -> b -> ByteString)
                -> b
                -> [Benchmark]
    encodeBench a fn b = [
        bgroup "BinaryProtocol" [
            bench "Pinch" $ nf (encode binaryProtocol) a
          , bench "Thrift" $ nf (fn (BinaryProtocol EmptyTransport)) b
          ]
      , bgroup "CompactProtocol" [
            bench "Pinch" $ nf (encode compactProtocol) a
          , bench "Thrift" $ nf (fn (CompactProtocol EmptyTransport)) b
          ]
      ]

doVector :: (Protocol p, Transport t)
         => (a -> ThriftVal)
         -> TypeMap
         -> p t
         -> Vector a
         -> ByteString
doVector from typemap pt vb = serializeVal pt (TList (T_STRUCT typemap) (map from $ Vector.toList vb))
