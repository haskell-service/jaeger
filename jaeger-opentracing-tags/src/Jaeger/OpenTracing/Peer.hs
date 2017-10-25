{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module:      Jaeger.OpenTracing.Peer
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: OverloadedStrings, TemplateHaskell
--
-- Generic representation of a network peer.

module Jaeger.OpenTracing.Peer (
      Peer
    , peer
    , peerAddress
    , peerHostname
    , peerIPv4
    , peerIPv6
    , peerPort
    , peerService
    -- * Reconstruct a 'Peer' from various values
    , fromSockAddr
    , fromSocket
    ) where

import Data.Int (Int64)

import Control.Monad.IO.Class (MonadIO, liftIO)

import Network.Socket (
    HostAddress, HostAddress6, PortNumber,
    SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix, SockAddrCan),
    Socket,
    getPeerName, hostAddressToTuple, hostAddress6ToTuple)

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Lens (
    Lens', (&), (.~), (?~), generateSignatures, lensRules, makeLensesWith)

import Formatting (Format, (%), (%.), bprint, hex, int, later, left, mapf, sformat)

-- | A 'Peer' represents a remote host or service.
data Peer = Peer { _peerAddress :: Maybe Text
                 , _peerHostname :: Maybe Text
                 , _peerIPv4 :: Maybe Text
                 , _peerIPv6 :: Maybe Text
                 , _peerPort :: Maybe Int64
                 , _peerService :: Maybe Text
                 }
    deriving (Show, Eq)

makeLensesWith (lensRules & generateSignatures .~ False) ''Peer

-- | The empty 'Peer'.
peer :: Peer
peer = Peer Nothing Nothing Nothing Nothing Nothing Nothing

-- | Remote "address", suitable for use in a networking client library.
--
-- This may be a "ip:port", a bare "hostname", a FQDN, or even a JDBC substring
-- like "mysql://prod-db:3306".
--
-- See also 'Jaeger.OpenTracing.Tags.peerAddress'.
peerAddress :: Lens' Peer (Maybe Text)

-- | Remote hostname.
--
-- E.g., "opentracing.io", "internal.dns.name".
--
-- See also 'Jaeger.OpenTracing.Tags.peerHostname'.
peerHostname :: Lens' Peer (Maybe Text)

-- | Remote IPv4 address as a .-separated tuple.
--
-- E.g., "127.0.0.1".
--
-- See also 'Jaeger.OpenTracing.Tags.peerIPv4'.
peerIPv4 :: Lens' Peer (Maybe Text)

-- | Remote IPv6 address as a string of colon-separated 4-char hex tuples.
--
-- E.g., "2001:0db8:85a3:0000:0000:8a2e:0370:7334".
--
-- See also 'Jaeger.OpenTracing.Tags.peerIPv6'.
peerIPv6 :: Lens' Peer (Maybe Text)

-- | Remote port.
--
-- E.g., 80.
--
-- See also 'Jaeger.OpenTracing.Tags.peerPort'.
peerPort :: Lens' Peer (Maybe Int64)

-- | Remote service name (for some unspecified definition of "service").
--
-- E.g., "elasticsearch", "a_custom_microservice", "memcache".
--
-- See also 'Jaeger.OpenTracing.Tags.peerService'.
peerService :: Lens' Peer (Maybe Text)

-- | Reconstruct a 'Peer' from a 'SockAddr'.
fromSockAddr :: SockAddr -> Peer
fromSockAddr sa = case sa of
    SockAddrInet p h -> peer & peerAddress ?~ sformat (ipv4 % ":" % portNumber) h p
                             & peerIPv4 ?~ sformat ipv4 h
                             & peerPort ?~ fromIntegral p
    SockAddrInet6 p _ h _ -> peer & peerAddress ?~ sformat ("[" % ipv6 % "]:" % portNumber) h p
                                  & peerIPv6 ?~ sformat ipv6 h
                                  & peerPort ?~ fromIntegral p
    SockAddrUnix p -> peer & peerAddress ?~ Text.pack p
    SockAddrCan _ -> error "fromSockAddr: address family 'AF_CAN' not supported"
  where
    portNumber :: Format r (PortNumber -> r)
    portNumber = mapf toInteger int
    ipv4 :: Format r (HostAddress -> r)
    ipv4 = later (\a -> let (a1, a2, a3, a4) = hostAddressToTuple a in
                        bprint (int % "." % int % "." % int % "." % int) a1 a2 a3 a4)
    ipv6 :: Format r (HostAddress6 -> r)
    ipv6 = later (\a -> let (a1, a2, a3, a4, a5, a6, a7, a8) = hostAddress6ToTuple a in
                        bprint
                            (h4 % ":" % h4 % ":" % h4 % ":" % h4 % ":" % h4 % ":" % h4 % ":" % h4 % ":" % h4)
                            a1 a2 a3 a4 a5 a6 a7 a8)
      where
        h4 = left 4 '0' %. hex

-- | Reconstruct a 'Peer' from a 'Socket'.
fromSocket :: MonadIO m => Socket -> m Peer
fromSocket = fmap fromSockAddr . liftIO . getPeerName
