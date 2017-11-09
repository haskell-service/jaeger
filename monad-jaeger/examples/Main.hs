{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever, forM_)
import Data.List (sortOn)

import Control.Concurrent.Lifted (killThread, threadDelay)

import Control.Exception.Safe (handleAny, throwString)

import qualified Data.Map as Map

import qualified Data.Text as Text

import Jaeger.Sampler (constSampler)
import Jaeger.Types (followsFrom)

import Jaeger.Process (process)
import Network.Jaeger (withJaegerLocal)

import Control.Monad.JaegerMetrics.Class (metricHelp, metricId)
import Control.Monad.JaegerTrace.Class (inSpan)
import Control.Monad.Trans.Jaeger (runJaegerT)
import Control.Monad.Trans.JaegerMetrics (runInMemoryMetricsT)
import Control.Monad.Trans.JaegerTrace (forkJaegerTraceT, runJaegerTraceT)

main :: IO ()
main = withJaegerLocal $ \sock -> do
    p <- process
    let sampler = constSampler True

    ((), (c, g)) <- (\act -> runInMemoryMetricsT $ runJaegerT act sock p sampler) $ flip runJaegerTraceT "demo" $ do
        threadDelay (50 * 1000)

        handleAny (const $ pure ()) $ inSpan "sub" $ do
            threadDelay (80 * 1000)

            inSpan "err" $ do
                threadDelay (20 * 1000)
                throwString "Massive system failure"

        tid <- (\act -> forkJaegerTraceT act "thread" followsFrom) $
            forever $ threadDelay (1000 * 1000)

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

    putStrLn "Collected metrics"
    putStrLn "-----------------"
    forM_ (sortOn fst $ Map.toList c) $ \(n, v) ->
        putStrLn $ help n ++ ": " ++ show v
    forM_ (sortOn fst $ Map.toList g) $ \(n, v) ->
        putStrLn $ help n ++ ": " ++ show v
  where
    help = Text.unpack . metricHelp . metricId
