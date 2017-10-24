{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module ThriftValues (spans, tags, process, batch) where

import Data.Vector (Vector)

import Jaeger_Types (
    Batch(Batch), Process(Process), Span(Span), SpanRef(SpanRef),
    SpanRefType(CHILD_OF), Tag(Tag), TagType(BOOL, STRING))

spans :: Vector Span
spans = [ Span 0 0 1 0 "GET" Nothing 0 0 0 Nothing Nothing
        , Span 0 0 2 0 "PUT" Nothing 0 0 0 Nothing Nothing
        , Span 0 0 3 0 "POST" (Just [SpanRef CHILD_OF 1 0 0]) 1 1 100 Nothing Nothing
        ]

tags :: Vector Tag
tags = [ Tag "version" STRING (Just "0.0.1") Nothing Nothing Nothing Nothing
       , Tag "production" BOOL Nothing Nothing (Just False) Nothing Nothing
       ]

process :: Process
process = Process "benchmark" (Just tags)

batch :: Batch
batch = Batch process spans
