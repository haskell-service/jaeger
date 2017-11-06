{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module:      Network.Wai.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: FlexibleContexts, OverloadedStrings, RankNTypes
--
-- Utilities to integrate <https://uber.github.com/jaeger/ Jaeger> tracing in
-- <https://hackage.haskell.com/package/wai Wai> applications.

module Network.Wai.Jaeger (
      runJaegerTraceRequest
    , runJaegerTraceApplication
    -- * OpenTracing 'Tag' utilities
    , requestTags
    , addRequestTags
    , responseTags
    , addResponseTags
    ) where

import Data.Monoid ((<>))

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadMask)

import Network.Wai (
    Application, Request, Response, ResponseReceived,
    isSecure, pathInfo, queryString, remoteHost,
    requestHeaders, requestHeaderHost, requestMethod, responseStatus)

import Network.HTTP.Types.Status (Status, statusCode)
import Network.HTTP.Types.URI (renderQuery)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Jaeger.OpenTracing.Peer (fromSockAddr)
import Jaeger.OpenTracing.Tags (
    SpanKind(Server), httpMethod, httpStatusCode, httpURL, peerTags, spanKind)
import Jaeger.Types (Tag, childOf, extract)

import Control.Monad.Jaeger.Class (MonadJaeger)
import Control.Monad.JaegerMetrics.Class (MonadJaegerMetrics)
import Control.Monad.JaegerTrace.Class (MonadJaegerTrace, addTags)
import Control.Monad.Trans.JaegerTrace (JaegerTraceT, continueJaegerTraceT, runJaegerTraceT)

-- | Common 'Jaeger.Types.spanOperationName' for root 'Span's of a 'Request' handler.
requestSpanOperationName :: Request -> Text
requestSpanOperationName req = "/" <> Text.intercalate "/" (pathInfo req)
{-# INLINE requestSpanOperationName #-}

-- | OpenTracing 'Tag's related to a 'Request'.
--
-- This includes 'httpMethod', 'httpURL', 'spanKind' and 'peerTags'.
--
-- /Note:/ 'httpURL' is reconstructed using heuristics, and not necessarily
-- equal to the original request URL.
requestTags :: Request -> [Tag]
requestTags req = [ httpMethod $ Text.decodeUtf8 $ requestMethod req
                  , httpURL $ mconcat [ if isSecure req then "https://" else "http://"
                                      , maybe "missing.host.header" Text.decodeUtf8 $ requestHeaderHost req
                                      , "/" <> Text.intercalate "/" (pathInfo req)
                                      , Text.decodeUtf8 $ renderQuery True $ queryString req
                                      ]
                  , spanKind Server
                  ] ++
                  peerTags (fromSockAddr $ remoteHost req)
{-# INLINE requestTags #-}

-- | Add 'requestTags' to the current 'Span'.
--
-- > addRequestTags = addTags . requestTags
addRequestTags :: MonadJaegerTrace m => Request -> m ()
addRequestTags = addTags . requestTags
{-# INLINE addRequestTags #-}

-- | OpenTracing 'Tag's related to a 'Response'
responseTags :: Status -> [Tag]
responseTags status = [ httpStatusCode $ fromIntegral $ statusCode status
                      ]
{-# INLINE responseTags #-}

-- | Add 'responseTags' to the current 'Span'.
--
-- > addResponseTags = addTags . responseTags
addResponseTags :: MonadJaegerTrace m => Status -> m ()
addResponseTags = addTags . responseTags
{-# INLINE addResponseTags #-}

-- | Run a 'JaegerTraceT' action in a prepopulated environment.
--
-- Primarily, this function will run the provided action using either
-- 'runJaegerTraceT' or 'continueJaegerTraceT', by trying to 'extract' a
-- 'SpanContext' out of the given 'Request' 'requestHeaders'.
--
-- Also, it will populate the root 'Span' 'Tag's using 'addRequestTags' before
-- running the given action.
--
-- /Note:/ Unlike 'runJaegerTraceApplication', any 'Response' data will /not/ be
-- added to the 'Span', because there's no way to access it. Use
-- 'addResponseTags' in some other place, if possible.
runJaegerTraceRequest :: (MonadBase IO m, MonadMask m, MonadJaeger m, MonadJaegerMetrics m)
                      => Request  -- ^ Request
                      -> JaegerTraceT m a  -- ^ Request handler
                      -> m a
runJaegerTraceRequest req act = case extract (requestHeaders req) of
    Left _ -> runJaegerTraceT act' operationName
    Right sc -> continueJaegerTraceT act' operationName childOf sc
  where
    operationName = requestSpanOperationName req
    act' = addRequestTags req >> act
{-# INLINE runJaegerTraceRequest #-}

-- | Turn an 'Network.Wai.Application'-like function into a 'Network.Wai.Application'.
--
-- This allows for 'Request' handlers to live in @'JaegerTraceT' m@. A user of
-- this function is required to pass a handler for @m@ actions, and turn them
-- into 'IO'.
--
-- 'addRequestTags' and 'addResponseTags' are used to populate root 'Span' tags.
runJaegerTraceApplication :: (MonadBase IO m, MonadMask m, MonadJaeger m, MonadJaegerMetrics m)
                          => (forall a. m a -> IO a)  -- ^ Handler for @m@ effects
                          -> (Request -> (Response -> JaegerTraceT m ResponseReceived) -> JaegerTraceT m ResponseReceived)  -- ^ 'Application'-like request handler
                          -> Application
runJaegerTraceApplication handle app req cont = handle $ runJaegerTraceRequest req (app req cont')
  where
    cont' resp = do
        addResponseTags (responseStatus resp)
        liftBase $ cont resp
