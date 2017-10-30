{-# LANGUAGE LambdaCase #-}

-- |
-- Module:      Network.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: LambdaCase
--
-- Utilities to connect to and interact with a Jaeger agent.

module Network.Jaeger (
    -- * Connecting to the Jaeger agent
      withJaeger
    -- ** Plumbing
    , Network.Jaeger.connect
    , close
    -- * Data transfer
    , sendBatch
    ) where

import Control.Monad (void)

import Network.Socket (
    AddrInfo(addrAddress, addrFamily, addrProtocol, addrSocketType),
    Socket, SocketType(Datagram),
    close, connect, defaultHints, getAddrInfo, socket, withSocketsDo)
import Network.Socket.ByteString (send)

import System.IO.Error (userError)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Exception.Safe (MonadMask, bracket, throwIO)

import Jaeger.Types (Batch, compactProtocol, emitBatch, encodeMessage)

-- | Connect to the Jaeger agent on @localhost:6831@.
--
-- /Note:/ This throws when address resolution fails.
connect :: IO Socket
connect = withSocketsDo $
    getAddrInfo (Just $ defaultHints { addrSocketType = Datagram }) (Just "localhost") (Just "6831") >>= \case
        [] -> throwIO $ userError "Network.Jaeger.connect: Failed to lookup localhost"
        (addr:_) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            Network.Socket.connect sock (addrAddress addr)
            return sock

-- | Create a connection to the Jaeger agent using 'connect', and close when the given action completes.
withJaeger :: (MonadIO m, MonadMask m) => (Socket -> m a) -> m a
withJaeger = bracket (liftIO Network.Jaeger.connect) (liftIO . close)

-- | Send a 'Batch' to the Jaeger agent connected to through the given 'Socket'.
sendBatch :: Socket -> Batch -> IO ()
sendBatch sock = void . send sock . encodeMessage compactProtocol . emitBatch
