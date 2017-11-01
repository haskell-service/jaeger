{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module:      Control.Monad.Jaeger.Class
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DefaultSignatures, TypeFamilies
--
-- @mtl@-style class of monads which can emit 'Span's to a Jaeger agent.

module Control.Monad.Jaeger.Class (
      MonadJaeger(..)
    ) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.List (ListT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Control.Monad.Trans.RWS.Lazy as Lazy (RWST)
import qualified Control.Monad.Trans.RWS.Strict as Strict (RWST)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as Lazy (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as Strict (WriterT)

import Data.Text (Text)

import Jaeger.Types (Span, Tag, TraceId)

class Monad m => MonadJaeger m where
    -- | Emit a 'Span' to a Jaeger agent.
    emitSpan :: Span -> m ()
    default emitSpan :: (MonadJaeger m', MonadTrans t, m ~ t m') => Span -> m ()
    emitSpan = lift . emitSpan

    -- | Decide whether to trace a request, likely using a 'Jaeger.Sampler.Sampler'.
    sample :: TraceId
           -> Text  -- ^ Root 'Span' 'Jaeger.Types.spanOperationName'
           -> m (Bool, [Tag])
    default sample :: (MonadJaeger m', MonadTrans t, m ~ t m') => TraceId -> Text -> m (Bool, [Tag])
    sample tid op = lift $ sample tid op

instance MonadJaeger m => MonadJaeger (ContT r m)
instance MonadJaeger m => MonadJaeger (ExceptT e m)
instance MonadJaeger m => MonadJaeger (IdentityT m)
instance MonadJaeger m => MonadJaeger (ListT m)
instance MonadJaeger m => MonadJaeger (MaybeT m)
instance (Monoid w, MonadJaeger m) => MonadJaeger (Lazy.RWST r w s m)
instance (Monoid w, MonadJaeger m) => MonadJaeger (Strict.RWST r w s m)
instance MonadJaeger m => MonadJaeger (ReaderT r m)
instance MonadJaeger m => MonadJaeger (ResourceT m)
instance MonadJaeger m => MonadJaeger (Lazy.StateT s m)
instance MonadJaeger m => MonadJaeger (Strict.StateT s m)
instance (Monoid w, MonadJaeger m) => MonadJaeger (Lazy.WriterT w m)
instance (Monoid w, MonadJaeger m) => MonadJaeger (Strict.WriterT w m)
