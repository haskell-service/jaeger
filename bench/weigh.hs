module Main (main) where

import Control.Lens ((^.), _Unwrapped)

import Pinch (encode)

import Weigh (func, mainWith, value)

import Jaeger (
    SpanId, binaryProtocol, compactProtocol, emitBatch, encodeMessage, traceId)

import Values (batch, process, spans, tags)

main :: IO ()
main = mainWith $ do
    value "value/SpanId 0" (0 ^. _Unwrapped :: SpanId)
    value "value/traceId 0 0" (traceId 0 0)
    value "value/tags" tags
    value "value/process" process
    value "value/spans" spans
    value "value/batch" batch
    func "emitBatch" emitBatch batch
    func "encode/binaryProtocol/tags" (encode binaryProtocol) tags
    func "encode/binaryProtocol/process" (encode binaryProtocol) process
    func "encode/binaryProtocol/spans" (encode binaryProtocol) spans
    func "encode/binaryProtocol/batch" (encode binaryProtocol) batch
    func "encode/compactProtocol/tags" (encode compactProtocol) tags
    func "encode/compactProtocol/process" (encode compactProtocol) process
    func "encode/compactProtocol/spans" (encode compactProtocol) spans
    func "encode/compactProtocol/batch" (encode compactProtocol) batch
    func "encodeMessage/binaryProtocol" (encodeMessage binaryProtocol . emitBatch) batch
    func "encodeMessage/compactProtocol" (encodeMessage compactProtocol . emitBatch) batch
