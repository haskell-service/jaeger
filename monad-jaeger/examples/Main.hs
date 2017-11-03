{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)

import Control.Concurrent.Lifted (fork, killThread, threadDelay)

import Control.Exception.Safe (handleAny, throwString)

import Jaeger.Sampler (constSampler)
import Jaeger.Types (SpanContext, extract, followsFrom)

import Jaeger.Process (process)
import Network.Jaeger (withJaeger)

import Control.Monad.JaegerTrace.Class (captureSpanContext, inSpan)
import Control.Monad.Trans.Jaeger (runJaegerT)
import Control.Monad.Trans.JaegerTrace (continueJaegerTraceT, runJaegerTraceT)

main :: IO ()
main = withJaeger $ \sock -> do
    p <- process
    let sampler = constSampler True

    (\act -> runJaegerT act sock p sampler) $ flip runJaegerTraceT "demo" $ do
        threadDelay (50 * 1000)

        sc <- captureSpanContext

        handleAny (const $ pure ()) $ inSpan "sub" $ do
            threadDelay (80 * 1000)

            inSpan "err" $ do
                threadDelay (20 * 1000)
                throwString "Massive system failure"

        tid <- lift $ fork $ do
            let ctx = maybe (Left "Unexpected Nothing") extract (sc :: Maybe SpanContext)
            either
                error
                (\sc' -> do
                    let act = forever $ threadDelay (1000 * 1000)
                    continueJaegerTraceT act "thread" followsFrom sc')
                ctx

        threadDelay (20 * 1000)

        inSpan "sub2" $
            threadDelay (30 * 1000)

        let loop 0 = pure ()
            loop n = inSpan "tower" $ do
                threadDelay (10 * 1000)
                loop (n - 1 :: Int)
                threadDelay (10 * 1000)
        loop 10

        threadDelay (10 * 1000)

        killThread tid
