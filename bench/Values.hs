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

import Jaeger hiding (process, batch)
import qualified Jaeger as J

spans :: [Span]
spans = [ span' trace0 (review _Wrapped 1) span0 "GET"
        , span' trace0 (review _Wrapped 2) span0 "PUT"
        , span' trace0 (review _Wrapped 3) span0 "POST"
            & spanReferences .~ [ spanRef childOf (traceId 1 0) span0
                                ]

            & spanFlags .~ [sampled]
            & spanStartTime .~ 1
            & spanDuration .~ 100
        ]
  where
    trace0 = traceId 0 0

tags :: [Tag]
tags = [ stringTag "version" "0.0.1"
       , boolTag "production" False
       ]

process :: Process
process = J.process "benchmark" tags

batch :: Batch
batch = J.batch process spans
