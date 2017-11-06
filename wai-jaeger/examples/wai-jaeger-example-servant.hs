{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (
      main
    ) where

import Data.Int (Int64)

import Control.Concurrent.Lifted (threadDelay)

import Servant (Get, JSON, Proxy(Proxy))
import Servant.Server (
    Handler(Handler), ServerT, (:~>)(NT), enter, errHTTPCode, errReasonPhrase, serve)

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (ExceptT(ExceptT))

import Control.Exception.Safe (try)

import qualified Data.ByteString.Char8 as BS

import Network.HTTP.Types.Status (mkStatus, status200)

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)

import System.Remote.Monitoring (forkServer, serverMetricStore)

import Control.Monad.JaegerTrace.Class (MonadJaegerTrace, addTags, inSpan)
import Control.Monad.Trans.Jaeger (runJaegerT)
import Control.Monad.Trans.JaegerMetrics (mkMetrics, runJaegerMetricsT)

import Jaeger.Process (process)
import Jaeger.Sampler (probabilisticSampler)
import Jaeger.Types (longTag)

import Network.Jaeger (withJaeger)

import Network.Wai.Jaeger (addResponseTags, runJaegerTraceRequest)

type API = Get '[JSON] Int

type JaegerServerT api m = (MonadBase IO m, MonadMask m, MonadJaegerTrace m) => ServerT api m

requestValue :: (MonadBase IO m, MonadMask m, MonadJaegerTrace m) => Int64 -> m Int64
requestValue v = inSpan "requestValue" $ do
    threadDelay $ 20 * 1000
    addTags [ longTag "value" v ]
    return (v `div` 2)

server :: JaegerServerT API m
server = do
    threadDelay $ 10 * 1000
    v <- requestValue 42
    threadDelay $ 10 * 1000
    return (2 * fromIntegral v)

api :: Proxy API
api = Proxy

main :: IO ()
main = do
    ekg <- forkServer "localhost" 8081
    putStrLn "EKG running on http://localhost:8081"
    metrics <- mkMetrics $ serverMetricStore ekg

    p <- process
    let sampler = probabilisticSampler 0.5
    withJaeger $ \sock -> do
        let settings = setPort 8080
                     $ setBeforeMainLoop (putStrLn "Server running on http://localhost:8080")
                       defaultSettings
            nt = jaegerToHandler sock p sampler metrics

            serve' a h req = serve a (enter (nt req) h) req

        runSettings settings $
            serve' api server
  where
    jaegerToHandler sock p sampler metrics req = NT $ \act ->
        let act' = try act >>= \res -> do
                addResponseTags $
                    either
                        (\e -> mkStatus (errHTTPCode e) (BS.pack $ errReasonPhrase e))
                        (const status200)
                        res
                pure res
        in
        Handler $ ExceptT $ runJaegerMetricsT (runJaegerT (runJaegerTraceRequest req act') sock p sampler) metrics
