{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jaeger.Types.TraceId (
      TraceId
    , traceId
    , traceIdHigh, traceIdLow
    ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import System.Random (Random(random, randomR))

import Control.Lens (
    Lens',
    (&), (.~),
    generateSignatures, lensRules, makeLensesWith)

-- | Representation of a trace ID.
data TraceId = TraceId { _traceIdLow :: !Int64
                       , _traceIdHigh :: !Int64
                       }
    deriving (Show, Eq, Ord, Generic)

makeLensesWith (lensRules & generateSignatures .~ False) ''TraceId
instance NFData TraceId

instance Random TraceId where
    random g =
        let (low, g') = random g in
        let (high, g'') = random g' in
        let t = TraceId { _traceIdLow = low
                        , _traceIdHigh = high
                        }
        in
        (t, g'')

    randomR (lo, hi) g =
        let (low, g') = randomR (_traceIdLow lo, _traceIdLow hi) g in
        let (high, g'') = randomR (_traceIdHigh lo, _traceIdHigh hi) g' in
        let t = TraceId { _traceIdLow = low
                        , _traceIdHigh = high
                        }
        in
        (t, g'')

-- | Construct a 'TraceId'.
traceId :: Int64 -> Int64 -> TraceId
traceId low high = TraceId { _traceIdLow = low
                           , _traceIdHigh = high
                           }
{-# INLINE traceId #-}

-- | The least significant 64 bits of a 'TraceId'.
traceIdLow :: Lens' TraceId Int64

-- | The most significant 64 bits of a 'TraceId'.
--
-- @0@ when only 64bit IDs are used.
traceIdHigh :: Lens' TraceId Int64
