{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

-- |
-- Module:      Servant.Jaeger.Client
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, UndecidableInstances
--
-- This module provides 'client' which can automatically generate querying
-- functions for each endpoint just from the type representing your API. These
-- HTTP calls will propagate trace metadata in HTTP headers if a trace is
-- ongoing.

module Servant.Jaeger.Client (
      client
    , ClientT
    , runClientT
    , ClientEnv(..)
    -- * Re-exports
    , module Servant.Client.Core.Reexport
    ) where

import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)

import Data.Functor.Alt (Alt((<!>)))

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader(ask, local, reader))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (
    MonadBaseControl(liftBaseWith, restoreM), StM, ComposeSt,
    defaultLiftBaseWith, defaultRestoreM,
    MonadTransControl(liftWith, restoreT), StT, defaultLiftWith, defaultRestoreT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), mapReaderT, runReaderT)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer.Class (MonadWriter)

import Control.Exception.Safe (MonadCatch, MonadMask, MonadThrow, catch, throw, try)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Encoding as Text (decodeUtf8)

import Data.Binary.Builder (toLazyByteString)

import Network.HTTP.Client (getUri)
import Network.HTTP.Types.Status (statusCode)

import Servant.Client (ClientEnv(..), runClientM)
import Servant.Client.Internal.HttpClient (requestToClientRequest)
import Servant.Client.Core (
    Client, ClientLike(mkClient), HasClient,
    RunClient(catchServantError, runRequest, throwServantError),
    ServantError, addHeader, clientIn, requestMethod, requestPath)
import Servant.Client.Core.Reexport

import Control.Monad.Jaeger.Class (MonadJaeger)
import Control.Monad.JaegerMetrics.Class (MonadJaegerMetrics)
import Control.Monad.JaegerTrace.Class (
    MonadJaegerTrace, addTag, addTags, captureSpanContext, inSpan)
import Jaeger.OpenTracing.Tags (
    SpanKind(Client), component, httpMethod, httpStatusCode, httpURL, spanKind)

-- | 'ClientT' is a monad transformer in which client functions run.
newtype ClientT m a = ClientT { unClientT :: ReaderT ClientEnv m a }
    deriving (
        Functor, Applicative, Monad,
        MonadTrans,
        MonadCont, MonadError e, MonadState s, MonadWriter w,
        MonadIO,
        MonadCatch, MonadMask, MonadThrow,
        MonadBase b,
        MonadJaeger,
        MonadJaegerMetrics,
        MonadJaegerTrace,
        Generic)

deriving instance MonadResource m => MonadResource (ClientT m)

instance MonadReader r m => MonadReader r (ClientT m) where
    ask = lift ask
    reader = lift . reader
    local = mapClientT . local

mapClientT :: (m a -> n b) -> ClientT m a -> ClientT n b
mapClientT f = ClientT . mapReaderT f . unClientT

deriving instance MonadRWS r w s m => MonadRWS r w s (ClientT m)

-- | Try clients in order, last error is preserved.
instance MonadCatch m => Alt (ClientT m) where
  a <!> b = catch a (\(_ :: ServantError) -> b)

instance MonadTransControl ClientT where
    type StT ClientT a = StT (ReaderT ClientEnv) a
    liftWith = defaultLiftWith ClientT unClientT
    restoreT = defaultRestoreT ClientT

instance MonadBaseControl b m => MonadBaseControl b (ClientT m) where
    type StM (ClientT m) a = ComposeSt ClientT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance (MonadBase IO m, MonadMask m, MonadJaegerTrace m) => RunClient (ClientT m) where
    runRequest req = ClientT $ do
        headers <- captureSpanContext
        env <- ask
        let path = Text.toStrict $ Text.decodeUtf8 $ toLazyByteString $ requestPath req
        inSpan path $ do
            let req' = foldr @[] (\(n, v) r -> addHeader n (v :: Text) r) req headers
            addTags [ component "servant-jaeger"
                    , httpMethod $ Text.pack $ show $ requestMethod req'
                    , httpURL $ Text.pack $ show $ getUri $ requestToClientRequest (baseUrl env) req'
                    , spanKind Client
                    ]
            res <- liftBase $ runClientM (runRequest req') env
            let tagSC = addTag . httpStatusCode . fromIntegral . statusCode . responseStatusCode
            case res of
                Left exc -> do
                    case exc of
                        FailureResponse res' -> tagSC res'
                        DecodeFailure _ res' -> tagSC res'
                        UnsupportedContentType _ res' -> tagSC res'
                        InvalidContentTypeHeader res' -> tagSC res'
                        ConnectionError _ -> pure ()
                    throw exc
                Right res' -> do
                    tagSC res'
                    pure res'

    throwServantError = throw
    catchServantError = catch

instance ClientLike (ClientT m a) (ClientT m a) where
    mkClient = id

-- | Generate a set of client functions for an API.
--
-- The second 'Proxy' argument determines the base monad in which the client
-- actions will run. You can likelly just pass 'Proxy' and let the compiler
-- figure things out.
--
-- See 'Servant.Client.client' for an example.
client :: HasClient (ClientT m) api => Proxy api -> Proxy m -> Client (ClientT m) api
client api proxy = case proxy of
    (Proxy :: Proxy m) -> clientIn api (Proxy :: Proxy (ClientT m))

-- | Run a 'ClientT' action, using the given 'ClientEnv' environment.
runClientT :: MonadCatch m => ClientT m a -> ClientEnv -> m (Either ServantError a)
runClientT ct env = try $ flip runReaderT env $ unClientT ct
