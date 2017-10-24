module Jaeger.Types.TimeSpec (
      TimeSpec
    , _TimeSpecUS
    ) where

import Data.Int (Int64)

import Control.Lens (Iso', iso)

import System.Clock (TimeSpec, fromNanoSecs, toNanoSecs)

-- | Unlawful 'Iso' between an 'Int64' (in microseconds) and a 'TimeSpec'.
--
-- /Note:/ This is not a lawful 'Iso' because it drops precision, so the
-- "'view'/'review' equals identity" law is broken.
_TimeSpecUS :: Iso' Int64 TimeSpec
_TimeSpecUS = iso
    (fromNanoSecs . fromIntegral . (* 1000))
    ((`div` 1000) . fromInteger . toNanoSecs)
{-# INLINE _TimeSpecUS #-}
