{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
      main
    ) where

import Control.Concurrent (threadDelay)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Text.PrettyPrint.Leijen.Text (Doc)

import Control.Monad.Log (
    MonadLog,
    Severity(Informational, Warning),
    WithCallStack, WithSeverity(WithSeverity), WithTimestamp,
    logMessage, runLoggingT, timestamp, withCallStack)

import Network.Jaeger (withJaeger)

import Jaeger.Process (process)
import Jaeger.Sampler (constSampler)

import Control.Monad.Trans.Jaeger (runJaegerT)
import Control.Monad.Trans.JaegerMetrics (runNoJaegerMetricsT)
import Control.Monad.Trans.JaegerTrace (runJaegerTraceT)

import Control.Monad.Log.Jaeger (
    handleCallStack, handleDoc, handleSeverity, handleTimestamp, handler)

app :: (MonadIO m, MonadLog (WithTimestamp (WithCallStack (WithSeverity Doc))) m) => m ()
app = do
    logMessage =<< timestamp (withCallStack (WithSeverity Informational "Informational message"))
    liftIO $ threadDelay (500 * 1000)
    logMessage =<< timestamp (withCallStack (WithSeverity Warning "Warning message"))

main :: IO ()
main = withJaeger $ \sock -> process >>= \proc ->
    runNoJaegerMetricsT $ runJaegerT (runJaegerTraceT (runLoggingT app (handler h)) "root") sock proc (constSampler True)
  where
    h = handleTimestamp $ handleCallStack $ handleSeverity $ handleDoc 0.4 80
