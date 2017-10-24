{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jaeger.Types.Batch (
      Batch
    , batch
    , batchProcess
    , batchSpans
    ) where

import GHC.Generics (Generic)

import Control.DeepSeq (NFData)

import Control.Lens (Lens', makeLenses)

import Pinch (Field, Pinchable, field, putField)

import Jaeger.Types.Process (Process)
import Jaeger.Types.Span (Span)

-- | 'Batch' is a collection of 'Span's reported out of 'Process'.
--
-- > struct Batch {
-- >   1: required Process    process
-- >   2: required list<Span> spans
-- > }
data Batch = Batch { _batchProcess' :: !(Field 1 Process)
                   , _batchSpans' :: !(Field 2 [Span])
                   }
    deriving (Show, Eq, Generic)

makeLenses ''Batch
instance NFData Batch
instance Pinchable Batch

-- | Construct a 'Batch'.
batch :: Process -> [Span] -> Batch
batch a b = Batch { _batchProcess' = putField a
                  , _batchSpans' = putField b
                  }
{-# INLINE batch #-}

-- | 'Batch' @process@.
batchProcess :: Lens' Batch Process
batchProcess = batchProcess' . field
{-# INLINE batchProcess #-}

-- | 'Batch' @spans@.
batchSpans :: Lens' Batch [Span]
batchSpans = batchSpans' . field
{-# INLINE batchSpans #-}


