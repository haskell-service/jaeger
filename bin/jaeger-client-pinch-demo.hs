{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}

module Main (main) where

import Control.Concurrent
import Control.Exception.Base
import Control.Monad
import Network.Socket hiding (send)
import Network.Socket.ByteString (send)
import System.Environment
import System.Random

import qualified Data.Set as Set
import qualified Data.Text as Text

import Control.Lens

import Jaeger

main :: IO ()
main = withSocketsDo $ do
    let hints = defaultHints { addrSocketType = Datagram }
    addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "6831")

    bracket (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
        connect sock (addrAddress addr)

        p <- process <$> fmap Text.pack getProgName <*> pure [stringTag "purpose" "demo"]
        tid <- randomIO
        sid1 <- randomIO

        let s0 = span' tid sid1 span0 "demo" & spanFlags %~ Set.insert debug
                                             & spanTags .~ [longTag "expected-duration" 8500]
        (s2, s1) <-
            flip timeSpan s0 $ do
                threadDelay 5000
                sid2 <- randomIO
                s2 <- flip timeSpan' (span' tid sid2 sid1 "lookup") $
                    threadDelay 2500
                threadDelay 1000
                return s2

        let msg = emitBatch $ batch p [s1, s2]
            bytes = encodeMessage compactProtocol msg

        void $ send sock bytes
