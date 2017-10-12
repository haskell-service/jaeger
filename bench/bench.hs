module Main (main) where

import Pinch (binaryProtocol, compactProtocol, encode, encodeMessage)

import Criterion (bench, bgroup, nf)
import Criterion.Main (defaultMain)

import Jaeger (emitBatch)

import Values (batch, process, spans, tags)

main :: IO ()
main = defaultMain [
    bench "emitBatch" $ nf emitBatch batch
  , bgroup "encode" [
        bgroup "binaryProtocol" $ encodeBench binaryProtocol
      , bgroup "compactProtocol" $ encodeBench compactProtocol
      ]
  , bgroup "encodeMessage" [
        bench "binaryProtocol" $ nf (encodeMessage binaryProtocol . emitBatch) batch
      , bench "compactProtocol" $ nf (encodeMessage compactProtocol . emitBatch) batch
      ]
  ]
  where
    encodeBench proto = [
        bench "tags" $ nf (encode proto) tags
      , bench "spans" $ nf (encode proto) spans
      , bench "process" $ nf (encode proto) process
      , bench "batch" $ nf (encode proto) batch
      ]
