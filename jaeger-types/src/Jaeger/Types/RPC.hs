{-# LANGUAGE BangPatterns #-}

module Jaeger.Types.RPC (
      emitBatch
    ) where

import qualified Data.Text as Text

import Pinch (Message, MessageType(Oneway), (.=), mkMessage, struct)

import Jaeger.Types.Batch (Batch)

-- | Construct an 'Agent.emitBatch' RPC request message.
--
-- > service Agent {
-- >     oneway void emitBatch(1: jaeger.Batch batch)
-- > }
emitBatch :: Batch  -- ^ Batch to emit
          -> Message
emitBatch !batch' = mkMessage (Text.pack "emitBatch") Oneway 0 $
    struct [ 1 .= batch' ]
{-# INLINE emitBatch #-}
