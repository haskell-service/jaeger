-- |
-- Module:      Jaeger.Clockk
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
--
-- Utilities to work with various clock sources when manipulating timing
-- information of Jaeger 'Span's.

module Jaeger.Clock (
      wallClockTime
    , TimeStamp
    , monotonicTime
    , diffTimeStamp
    ) where

import System.Clock (Clock(Boottime, Realtime), TimeSpec, diffTimeSpec, getTime)

-- | Retrieve the current wall-clock time (used for e.g. 'Jaeger.Types.spanStartTime').
--
-- This clock is affected by NTP or other time changes and jumps. It should not
-- be used to merely calculate the amount of time some action takes to
-- complete.
wallClockTime :: IO TimeSpec
wallClockTime = getTime Realtime
{-# INLINE wallClockTime #-}

-- | A 'TimeStamp' is a point in time which is unrelated to wall-clock time.
newtype TimeStamp = TimeStamp { getTimeStamp :: TimeSpec }
    deriving (Show, Eq)

-- | Retrieve a current monotonic time, valid only during process lifetime (used to calculate e.g. 'Jaeger.Types.spanDuration').
--
-- This clock is not affected by NTP or other time changes and jumps, but has no
-- value related to wall-clock time.
monotonicTime :: IO TimeStamp
monotonicTime = TimeStamp <$> getTime Boottime
{-# INLINE monotonicTime #-}

-- | Calculate the absolute difference between two 'TimeStamp's.
--
-- This can be passed to e.g. 'Jaeger.Types.spanDuration' when begin- and
-- end-time of some action inside a 'Jaeger.Types.Span' have been sampled using
-- 'monotonicTime'.
diffTimeStamp :: TimeStamp -> TimeStamp -> TimeSpec
diffTimeStamp end start = diffTimeSpec (getTimeStamp end) (getTimeStamp start)
{-# INLINE diffTimeStamp #-}
