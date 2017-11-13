{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module:      Servant.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: DataKinds, FlexibleContexts, RankNTypes, ScopedTypeVariables
--
-- Utilities to integrate <https://uber.github.com/jaeger/ Jaeger> tracing in
-- <https://hackage.haskell.com/package/servant Servant> applications.

module Servant.Jaeger (
      JaegerServerT
    -- * Run a Wai 'Application' from an API and a 'JaegerServerT' implementing it
    , Servant.Jaeger.serve
    , Servant.Jaeger.serveWithContext
    -- * Natural transformation between a Jaeger stack and a 'Handler'
    , jaegerToHandler
    ) where

import Data.Proxy (Proxy(Proxy))

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT(ExceptT))

import Control.Exception.Safe (try)

import qualified Data.ByteString.Char8 as BS8

import Network.HTTP.Types.Status (mkStatus, status200)

import Servant.Server (
    Context, Handler(Handler), HasServer, ServerT,
    errHTTPCode, errReasonPhrase, hoistServer, hoistServerWithContext, serve, serveWithContext)

import Network.Socket (Socket)

import Network.Wai (Application, Request)

import Jaeger.Sampler (Sampler)
import Jaeger.Types (Process)

import Control.Monad.JaegerMetrics.Class (MonadJaegerMetrics)
import Control.Monad.Trans.Jaeger (JaegerT, runJaegerT)
import Control.Monad.Trans.JaegerTrace (JaegerTraceT)

import Network.Wai.Jaeger (addResponseTags, runJaegerTraceRequest)

-- | Construct (per-'Request') a natural transformation between a Jaeger stack and a 'Handler'.
--
-- This is exported for plumbing purposes, mostly.
jaegerToHandler :: ( MonadBase IO m
                   , MonadMask m
                   , MonadJaegerMetrics m
                   )
                => Socket  -- ^ Socket to a Jaeger agent, passed to 'runJaegerT'
                -> Process  -- ^ Traced 'Process', passed to 'runJaegerT'
                -> Sampler IO  -- ^ Tracing 'Sampler', passed to 'runJaegerT'
                -> (forall x. m x -> IO x)  -- ^ Handler for 'MonadJaegerMetrics' and other effects
                -> Request  -- ^ 'Request' being handled, passed to 'runJaegerTraceRequest'
                -> (forall x. JaegerTraceT (JaegerT m) x -> Handler x)
jaegerToHandler sock proc sampler handleMetrics req act =
    let act' = try act >>= \res -> do
            let status = either servantErrToStatus (const status200) res
            addResponseTags status
            pure res
        runJaeger = handleMetrics (runJaegerT (runJaegerTraceRequest req act') sock proc sampler)
    in
    Handler $ ExceptT runJaeger
  where
    servantErrToStatus err = mkStatus (errHTTPCode err) (BS8.pack $ errReasonPhrase err)

-- | A "Servant" 'Servant.Server.Server' which runs in a 'MonadJaegerTrace' context.
type JaegerServerT api m = (MonadBase IO m, MonadMask m, MonadJaegerMetrics m) => ServerT api (JaegerTraceT (JaegerT m))

-- | Similar to 'Servant.Server.serve', but run a 'JaegerServerT'.
--
-- The first arguments are used to run 'JaegerT' and 'JaegerMetricsT' effects.
serve :: ( HasServer api '[]
         , MonadBase IO m
         , MonadMask m
         , MonadJaegerMetrics m
         )
      => Socket  -- ^ Socket to a Jaeger agent, passed to 'runJaegerT'
      -> Process  -- ^ Traced 'Process', passed to 'runJaegerT'
      -> Sampler IO  -- ^ Tracing 'Sampler', passed to 'runJaegerT'
      -> (forall x. m x -> IO x)  -- ^ Handler for 'MonadJaegerMetrics' and other effects
      -> Proxy api
      -> JaegerServerT api m
      -> Application
serve sock proc sampler metrics api server =
    let nt = jaegerToHandler sock proc sampler metrics in
    \req -> Servant.Server.serve api (hoistServer api (nt req) server) req

-- | Similar to 'Servant.Server.serveWithContext', but run a 'JaegerServerT'.
--
-- The first arguments are used to run 'JaegerT' and 'JaegerMetricsT' effects.
serveWithContext :: forall api context m.
                    ( HasServer api context
                    , MonadBase IO m
                    , MonadMask m
                    , MonadJaegerMetrics m
                    )
                 => Socket  -- ^ Socket to a Jaeger agent, passed to 'runJaegerT'
                 -> Process  -- ^ Traced 'Process', passed to 'runJaegerT'
                 -> Sampler IO  -- ^ Tracing 'Sampler', passed to 'runJaegerT'
                 -> (forall a. m a -> IO a)  -- ^ Handler for 'MonadJaegerMetrics' and other effects
                 -> Proxy api
                 -> Context context
                 -> JaegerServerT api m
                 -> Application
serveWithContext sock proc sampler metrics api context server =
    let nt = jaegerToHandler sock proc sampler metrics in
    \req -> Servant.Server.serveWithContext api context (hoistServerWithContext api (Proxy :: Proxy context) (nt req) server) req
