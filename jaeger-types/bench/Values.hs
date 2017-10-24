{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Values (
      tags
    , spans
    , process
    , batch
    ) where

import Control.Lens ((&), (.~), _Wrapped, review)

import Jaeger.Types hiding (process, batch)
import qualified Jaeger.Types as J

spans :: [Span]
spans = [ span' trace0 (review _Wrapped 1) Nothing "GET"
        , span' trace0 (review _Wrapped 2) Nothing "PUT"
        , span' trace0 (review _Wrapped 3) Nothing "POST"
            & spanReferences .~ [ spanRef childOf (traceId 1 0) span0
                                ]

            & spanFlags .~ [sampled]
            & spanStartTime .~ 1
            & spanDuration .~ 100
        ]
  where
    span0 = review _Wrapped 0
    trace0 = traceId 0 0

tags :: [Tag]
tags = [ stringTag "version" "0.0.1"
       , boolTag "production" False
       ]

process :: Process
process = J.process "benchmark" tags

batch :: Batch
batch = J.batch process spans
