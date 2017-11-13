{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
      main
    ) where

import Data.Proxy (Proxy(Proxy))

import Control.Concurrent.Lifted (threadDelay)

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger (LogLevel(LevelInfo), logInfoSH, runNoLoggingT)

import Servant.API (Get, JSON)

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)

import Prometheus (buildRegistry, publishRegistryMiddleware)
import Prometheus.GHC (ghcStats)
import Network.Wai.Middleware.Prometheus (instrumentRequests)

import Control.Monad.JaegerTrace.Class (MonadJaegerTrace, addTags, inSpan)
import Control.Monad.Logger.Jaeger (runJaegerLoggingT)
import Control.Monad.Trans.JaegerMetrics.Prometheus (mkMetrics, runPrometheusMetricsT)

import Jaeger.Process (process)
import Jaeger.Sampler (probabilisticSampler)
import Jaeger.Types (longTag)

import Network.Jaeger (withJaegerLocal)

import Servant.Jaeger.Server (JaegerServerT, serve)

-- | Our API specification.
type API = Get '[JSON] Int

-- | Remote source query function.
--
-- Consider this to be some library function, not related to Servant
requestValue :: (MonadBase IO m, MonadMask m, MonadJaegerTrace m) => m Int
requestValue = inSpan "requestValue" $ do
    threadDelay $ 20 * 1000
    let v = 42
    addTags [ longTag "value" $ fromIntegral v ]
    return v

-- | Implementation of 'API', using 'MonadJaegerTrace'.
--
-- Also integrating "monad-logger-jaeger" logging.
server :: JaegerServerT API m
server = runNoLoggingT $ flip runJaegerLoggingT LevelInfo $ do
    threadDelay $ 10 * 1000
    v <- requestValue
    $(logInfoSH) ("The value is" :: String, v)
    threadDelay $ 10 * 1000
    return v

api :: Proxy API
api = Proxy

main :: IO ()
main = do
    ((metrics, waiMetrics), registry) <- buildRegistry $ do
        metrics <- mkMetrics
        _ <- ghcStats
        waiMetrics <- instrumentRequests
        return (metrics, waiMetrics)

    withJaegerLocal $ \sock -> do
        p <- process
        let sampler = probabilisticSampler 0.5

        let app = serve sock p sampler (flip runPrometheusMetricsT metrics) api server

        let settings = setPort 8080
                     $ setBeforeMainLoop (do
                        putStrLn "Server running on http://localhost:8080"
                        putStrLn "Metrics at http://localhost:8080/metrics")
                       defaultSettings

        runSettings settings $ waiMetrics
                             $ publishRegistryMiddleware ["metrics"] registry
                               app
