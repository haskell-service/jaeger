{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Main (main) where

import Control.Lens

import Pinch (binaryProtocol, compactProtocol, encodeMessage)

import Criterion (bench, nf)
import Criterion.Main (defaultMain)

import Jaeger

main :: IO ()
main = defaultMain [
      bench "encodeMessage binaryProtocol" $ nf (encodeMessage binaryProtocol . emitBatch) msg
    , bench "encodeMessage compactProtocol" $ nf (encodeMessage compactProtocol . emitBatch) msg
    ]
  where
    msg = batch (process "benchmark" [ stringTag "version" "0.0.1"
                                     , boolTag "production" True
                                     ])
                [ span' trace0 (review _Wrapped 1) span0 "GET"
                , span' trace0 (review _Wrapped 2) span0 "PUT"
                , span' trace0 (review _Wrapped 3) span0 "POST"
                    & spanReferences .~ [ spanRef childOf (traceId 1 0) (review _Wrapped 0)
                                        ]
                    & spanFlags .~ [sampled]
                    & spanStartTime .~ 1
                    & spanDuration .~ 100
                ]
    trace0 = traceId 0 0
