{-# LANGUAGE DeriveGeneric #-}

module Jaeger.Types.Flag (
      Flag
    , debug, sampled
    , flags
    ) where

import Data.Bits (Bits, setBit, testBit, zeroBits)
import Data.Int (Int32)
import Data.List (foldl')
import Data.Word (Word8)
import GHC.Generics (Generic)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Lens (Iso', iso)

-- | Representation of a 'Span' flag (see 'spanFlags').
data Flag = Sampled | Debug
    deriving (Show, Eq, Enum, Bounded, Ord, Generic)

-- | A sampled 'Span'.
sampled :: Flag
sampled = Sampled

-- | A debug 'Span'.
debug :: Flag
debug = Debug

flags :: Bits a => Iso' a (Set Flag)
flags = iso toSet fromSet
  where
    toSet a = foldl' (\s (b, f) -> if testBit a b then Set.insert f s else s) Set.empty bits
    fromSet s = foldl' (\a (b, f) -> if Set.member f s then setBit a b else a) zeroBits bits
    bits = [ (0, Sampled)
           , (1, Debug)
           ]
{-# INLINE flags #-}
{-# SPECIALIZE flags :: Iso' Word8 (Set Flag) #-}
{-# SPECIALIZE flags :: Iso' Int32 (Set Flag) #-}
