{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Jaeger.Sampler
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: OverloadedStrings
--
-- Basic 'Sampler' definitions for Jaeger tracing.

module Jaeger.Sampler (
      Sampler
    , constSampler
    , probabilisticSampler
    ) where

import Data.Text (Text)

import Control.Lens ((^.))

import Jaeger.Types (Tag, TraceId, boolTag, doubleTag, stringTag, traceIdLow)

-- | A type alias for sampler decision functions.
--
-- Given a 'TraceId' and 'Jaeger.Types.spanOperationName', the sampler decides
-- whether or not to trace and report the current request, and passes a set of
-- 'Tag's to append to the root 'Jaeger.Types.Span' of the trace.
--
-- If the 'Sampler' requires certain effects, these can be provided by 'm'.
type Sampler m = TraceId -> Text -> m (Bool, [Tag])

-- Note: Rationale for 'm'
-- This allows for pure Samplers to be tested (in Identity or whatnot), and
-- allows for impure samplers to run in IO.
-- It's up to the user of the sampler to pick some 'm', see e.g. how
-- monad-jaeger handles things.

-- | The @const@ sampler.
--
-- This 'Sampler' always returns the given result.
constSampler :: Applicative m => Bool -> Sampler m
constSampler d = \_ _ -> pure (d, tags)
  where
    tags = [ stringTag "sampler.type" "const"
           , boolTag "sampler.param" d
           ]

-- | A simple @probabilistic@ sampler.
--
-- Assuming a uniform distribution of trace IDs, this 'Sampler' samples at a given rate.
-- E.g. to trace (approximately) 1% of all requests, use
--
-- > probabilisticSampler 0.01
probabilisticSampler :: Applicative m => Rational -> Sampler m
probabilisticSampler r
    | r < 0 || r > 1 = error $ "Sampling rate must be between 0.0 and 1.0, got " ++ show r
    | r == 0 = \_ _ -> pure (False, tags)
    | r == 1 = \_ _ -> pure (True, tags)
    | otherwise =
        let mib = minBound in
        let rangeSize = abs (toInteger mib) + abs (toInteger $ maxBound `asTypeOf` mib) in
        let cutOff = mib + floor (fromInteger rangeSize * r) in
        \tid _ -> pure (tid ^. traceIdLow <= cutOff, tags)
  where
    tags = [ stringTag "sampler.type" "probabilistic"
           , doubleTag "sampler.param" $ fromRational r
           ]
