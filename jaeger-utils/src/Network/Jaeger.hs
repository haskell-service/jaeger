{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module:      Network.Jaeger
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: FlexibleContexts, LambdaCase
--
-- Utilities to connect to and interact with a Jaeger agent.

module Network.Jaeger (
    -- * Connecting to the Jaeger agent
      withJaeger
    , withJaegerLocal
    , withJaegerEnv
    -- ** Plumbing
    , Network.Jaeger.connect
    , connectLocal
    , connectEnv
    , close
    -- * Data transfer
    , sendBatch
    ) where

import Control.Monad (join, void)
import Data.Word (Word16)
import System.Environment (getEnv)

import Network.Socket (
    AddrInfo(addrAddress, addrFamily, addrProtocol, addrSocketType),
    Socket, SocketType(Datagram),
    close, connect, defaultHints, getAddrInfo, socket, withSocketsDo)
import Network.Socket.ByteString (send)

import System.IO.Error (userError)

import Control.Monad.Base (MonadBase, liftBase)

import Control.Exception.Safe (MonadMask, bracket, throwIO)

import Jaeger.Types (Batch, compactProtocol, emitBatch, encodeMessage)

-- | Connect to the Jaeger agent on given host and port.
--
-- /Note:/ This throws when address resolution fails.
connect :: String -> Word16 -> IO Socket
connect host port = withSocketsDo $
    getAddrInfo (Just $ defaultHints { addrSocketType = Datagram }) (Just host) (Just $ show port) >>= \case
        [] -> throwIO $ userError "Network.Jaeger.connect: Failed to lookup localhost"
        (addr:_) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            Network.Socket.connect sock (addrAddress addr)
            return sock

-- | Connect to the Jager agent on @localhost@ using the default port (@6831@).
--
-- Uses 'connect' underneath.
connectLocal :: IO Socket
connectLocal = Network.Jaeger.connect "localhost" 6831

-- | Connect to the Jaeger agent configured in the process environment.
--
-- This retrieves the agent host from @JAEGER_AGENT_HOST@ and the port from
-- @JAEGER_AGENT_PORT@. Note this will throw exceptions if any of these
-- environment variables is not set.
--
-- Uses 'connect' underneath.
connectEnv :: IO Socket
connectEnv = join $ Network.Jaeger.connect <$> getEnv "JAEGER_AGENT_HOST" <*> fmap read (getEnv "JAEGER_AGENT_PORT")

-- | Create a connection to a Jaeger agent using 'connect', and close when the given action completes.
withJaeger :: (MonadBase IO m, MonadMask m) => String -> Word16 -> (Socket -> m a) -> m a
withJaeger host port = bracket (liftBase $ Network.Jaeger.connect host port) (liftBase . close)

-- | Create a connection to the local Jaeger agent using 'connectLocal', and close when the given action completes.
withJaegerLocal :: (MonadBase IO m, MonadMask m) => (Socket -> m a) -> m a
withJaegerLocal = bracket (liftBase connectLocal) (liftBase . close)

-- | Create a connection to a Jaeger agent using 'connectEnv', and close when the given action completes.
withJaegerEnv :: (MonadBase IO m, MonadMask m) => (Socket -> m a) -> m a
withJaegerEnv = bracket (liftBase connectEnv) (liftBase . close)

-- | Send a 'Batch' to the Jaeger agent connected to through the given 'Socket'.
sendBatch :: Socket -> Batch -> IO ()
sendBatch sock = void . send sock . encodeMessage compactProtocol . emitBatch
