{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main (
      main
    ) where

import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import System.Environment (getArgs)

import Control.Concurrent.Lifted (threadDelay)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)

import Control.Lens ((&), (%~))

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Catch (MonadMask, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LogLevel(LevelInfo), logInfoSH, runNoLoggingT)

import Servant.API ((:>), (:<|>)((:<|>)), Get, JSON, Post, ReqBody)

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)

import System.Remote.Monitoring (forkServer, serverMetricStore)

import Network.HTTP.Client (defaultManagerSettings, newManager)

import Control.Monad.JaegerTrace.Class (MonadJaegerTrace, addTags, inSpan)
import Control.Monad.Logger.Jaeger (runJaegerLoggingT)
import Control.Monad.Trans.Jaeger (runJaegerT)
import Control.Monad.Trans.JaegerMetrics (runNoJaegerMetrics)
import Control.Monad.Trans.JaegerMetrics.EKG (mkMetrics, runEKGMetricsT)
import Control.Monad.Trans.JaegerTrace (runJaegerTraceT)

import Jaeger.Process (process)
import Jaeger.Sampler (constSampler, probabilisticSampler)
import Jaeger.Types (longTag, processServiceName)

import Network.Jaeger (withJaegerLocal)

import Servant.Jaeger.Client (ClientEnv(ClientEnv), client, parseBaseUrl, runClientT)
import Servant.Jaeger.Server (JaegerServerT, serve)

-- | Our API specification.
type API = "value" :>  (  Get '[JSON] Int
                     :<|> ReqBody '[JSON] Int :> Post '[JSON] ()
                       )

-- | Remote source query function.
--
-- Consider this to be some library function, not related to Servant
requestValue :: (MonadBase IO m, MonadMask m, MonadJaegerTrace m) => TVar Int -> m Int
requestValue tv = inSpan "requestValue" $ do
    threadDelay $ 20 * 1000
    v <- liftBase $ readTVarIO tv
    addTags [ longTag "value" $ fromIntegral v ]
    return v

-- | Implementation of 'API', using 'MonadJaegerTrace'.
--
-- Also integrating "monad-logger-jaeger" logging.
server :: TVar Int -> JaegerServerT API m
server tv = handleGet :<|> handlePost
  where
    run = runNoLoggingT . flip runJaegerLoggingT LevelInfo
    handleGet = run $ do
        threadDelay $ 10 * 1000
        v <- requestValue tv
        $(logInfoSH) ("The value is" :: String, v)
        threadDelay $ 10 * 1000
        return v
    handlePost i = run $ do
        threadDelay $ 5 * 1000
        old <- liftBase $ atomically $ do
            old <- readTVar tv
            writeTVar tv i
            return old
        $(logInfoSH) ("Updated value from" :: String, old, "to" :: String, i)

api :: Proxy API
api = Proxy

runServer :: IO ()
runServer = do
    ekg <- forkServer "localhost" 8081
    putStrLn "EKG running on http://localhost:8081"

    withJaegerLocal $ \sock -> do
        p <- process
        let sampler = probabilisticSampler 0.5
        metrics <- mkMetrics $ serverMetricStore ekg

        tv <- newTVarIO 42

        let app = serve sock p sampler (flip runEKGMetricsT metrics) api (server tv)

        let settings = setPort 8080
                     $ setBeforeMainLoop (putStrLn "Server running on http://localhost:8080")
                       defaultSettings

        runSettings settings app

runClient :: IO ()
runClient = withJaegerLocal $ \sock -> process >>= \proc -> do
    let proc' = proc & processServiceName %~ (<> "-client")
    runNoJaegerMetrics $ (\act -> runJaegerT act sock proc' (constSampler True)) $ flip runJaegerTraceT "client" $ do
        env <- ClientEnv <$> liftIO (newManager defaultManagerSettings)
                         <*> parseBaseUrl "http://localhost:8080/"
        res <-  flip runClientT env $ do
            r1 <- getValue
            postValue (2 * r1)
            r2 <- getValue
            return (r1, r2)

        either throwM (liftIO . print) res
  where
    getValue :<|> postValue = client api Proxy

main :: IO ()
main = getArgs >>= \case
    ["server"] -> runServer
    ["client"] -> runClient
    _ -> error "Unknown command"
