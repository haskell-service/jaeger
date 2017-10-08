{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Jaeger.Utils (
      TimeStamp
    , Clock(..)
    , monotonicTime
    , diffTime
    , wallClockTime
    , timeSpec
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import System.Clock (TimeSpec, diffTimeSpec, getTime)
import qualified System.Clock as Clock

-- Kind of clock.
data Clock = WallClock | Monotonic

-- Timestamp from a specific kind of 'Clock'.
newtype TimeStamp (a :: Clock) = TimeStamp { unTimeStamp :: TimeSpec }
    deriving (Show, Eq, Ord)

-- Wall-clock time (aka realtime).
--
-- This is an absolute point in time, which can be trivially converted in a
-- 'TimeSpec'.
wallClockTime :: MonadIO m => m (TimeStamp 'WallClock)
wallClockTime = liftIO $ TimeStamp <$> getTime Clock.Realtime
{-# INLINABLE wallClockTime #-}

-- Retrieve an absolute point in time.
timeSpec :: TimeStamp 'WallClock -> TimeSpec
timeSpec = unTimeStamp
{-# INLINE timeSpec #-}

-- Monotonic time.
--
-- This is a point in time taken from a clock whose time increases
-- monotonically, but was started at some unspecified point in time, i.e. its
-- value is not suitable to represent a 'human' timestamp.
--
-- /Note:/ currently uses 'Clock.Boottime'.
monotonicTime :: MonadIO m => m (TimeStamp 'Monotonic)
monotonicTime = liftIO $ TimeStamp <$> getTime Clock.Boottime
{-# INLINABLE monotonicTime #-}

-- Calculate the absolute difference between two monotonic clock timestamps.
diffTime :: TimeStamp 'Monotonic -> TimeStamp 'Monotonic -> TimeSpec
diffTime (TimeStamp a) (TimeStamp b) = diffTimeSpec a b
{-# INLINE diffTime #-}
