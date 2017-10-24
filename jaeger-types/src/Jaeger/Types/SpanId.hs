{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Jaeger.Types.SpanId (
      SpanId
    ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Control.Lens (makeWrapped)

import System.Random (Random(random, randomR))

-- | Representation of a span ID.
newtype SpanId = SpanId Int64
    deriving (Show, Eq, Ord, Generic)

makeWrapped ''SpanId
instance NFData SpanId

instance Random SpanId where
    random g = let (a, g') = random g in (SpanId a, g')
    randomR (SpanId l, SpanId h) g = let (a, g') = randomR (l, h) g in (SpanId a, g')
