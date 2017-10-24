{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Jaeger.Types.Process (
      Process
    , process
    , processServiceName
    , processTags
    ) where

import GHC.Generics (Generic)

import Data.Text (Text)

import Control.DeepSeq (NFData)

import Control.Lens (Lens', _Empty, makeLenses, non')

import Pinch (Field, Pinchable, field, putField)

import Jaeger.Types.Tag (Tag)

-- | 'Process' describes the traced process/service that emits 'Span's.
--
-- > struct Process {
-- >   1: required string    serviceName
-- >   2: optional list<Tag> tags
-- > }
data Process = Process { _processServiceName' :: !(Field 1 Text)
                       , _processTags' :: !(Field 2 (Maybe [Tag]))
                       }
    deriving (Show, Eq, Generic)

makeLenses ''Process
instance NFData Process
instance Pinchable Process

-- | Construct a 'Process'.
process :: Text -> [Tag] -> Process
process a b = Process { _processServiceName' = putField a
                      , _processTags' = putField $ if null b then Nothing else Just b
                      }
{-# INLINE process #-}

-- | 'Process' @serviceName@.
processServiceName :: Lens' Process Text
processServiceName = processServiceName' . field
{-# INLINE processServiceName #-}

-- | 'Process' @tags@.
--
-- /Note:/ This is an 'optional' value in the Thrift message, which is mapped
-- transparantly from/to an empty list.
processTags :: Lens' Process [Tag]
processTags = processTags' . field . non' _Empty
{-# INLINE processTags #-}
