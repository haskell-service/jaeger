{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jaeger.Types.Log (
      Log
    , Jaeger.Types.Log.log
    , logTimestamp, logFields
    ) where

import Data.Int (Int64)
import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Control.Lens (Lens', (^.), makeLenses, re)

import Pinch (Field, Pinchable, field, putField)

import Jaeger.Types.Tag (Tag)
import Jaeger.Types.TimeSpec (TimeSpec, _TimeSpecUS)

-- | 'Log' is a timed event with an arbitrary set of 'Tag's.
--
-- > struct Log {
-- >   1: required i64       timestamp
-- >   2: required list<Tag> fields
-- > }
data Log = Log { _logTimestamp' :: !(Field 1 Int64)
               , _logFields' :: !(Field 2 [Tag])
               }
    deriving (Show, Eq, Generic)

makeLenses ''Log
instance NFData Log
instance Pinchable Log

-- | Construct a 'Log'.
--
-- /Note:/ The given 'TimeSpec' is rounded down to microsecond precision. See
-- 'logTimestamp'.
log :: TimeSpec -> [Tag] -> Log
log a b = Log { _logTimestamp' = putField (a ^. re _TimeSpecUS)
              , _logFields' = putField b
              }
{-# INLINE log #-}

-- | 'Log' @timestamp@.
--
-- /Note:/ This is an unlawful 'Lens' because it breaks the first law, "You get
-- back what you put in": the precision of a provided 'TimeSpec' is dropped to
-- microseconds, so upon retrieval a different value can be returned.
logTimestamp :: Lens' Log TimeSpec
logTimestamp = logTimestamp' . field . _TimeSpecUS
{-# INLINE logTimestamp #-}

-- | 'Log' @fields@.
logFields :: Lens' Log [Tag]
logFields = logFields' . field
{-# INLINE logFields #-}
