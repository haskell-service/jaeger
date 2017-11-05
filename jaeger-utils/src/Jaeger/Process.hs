{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#include "cabal_macros.h"

-- |
-- Module:      Jaeger.Process
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
--
-- Maintainer:  ikke@nicolast.be
-- Stability:   alpha
-- Portability: CPP, OverloadedStrings
--
-- Utilities to create 'Process' values, based on the running process'
-- environment.

module Jaeger.Process (
      Jaeger.Process.process
    , processTags
    ) where

import Data.List (sortBy)
import Data.Maybe (catMaybes)
import System.Environment (getProgName)

import System.Posix.Process (getProcessID, getParentProcessID)
import System.Posix.Unistd (SystemID(systemName, nodeName, release, version, machine), getSystemID)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (toStrict)
import qualified Data.Text.Lazy.Builder as Text (toLazyText)
import Data.Text.Lazy.Builder.Int (decimal, hexadecimal)

import Network.Socket (SockAddr(SockAddrInet, SockAddrInet6), hostAddressToTuple, hostAddress6ToTuple)

import qualified Data.Set as Set

import Jaeger.Types (Process, Tag, longTag, process, stringTag)

import Network.Ifaddrs (IFF(IFF_LOOPBACK, IFF_RUNNING, IFF_UP), Ifaddrs(ifaAddr, ifaFlags), getInetInterfaces)

-- | Construct a 'Process', setting the service name to the process name and tags to 'processTags'.
process :: IO Process
process = Jaeger.Types.process <$> fmap Text.pack getProgName
                               <*> processTags

-- | Generate some generic 'Process' 'Tag's.
processTags :: IO [Tag]
processTags = (++) <$> stdTags <*> fmap catMaybes maybeTags
  where
    stdTags = sequence [ longTag "process.pid" <$> fmap fromIntegral getProcessID
                       , longTag "process.parentPid" <$> fmap fromIntegral getParentProcessID
                       , stringTag "process.uname" <$> getUname
                       , pure $ stringTag "jaeger.version" VERSION_jaeger_types
                       , stringTag "hostname" . Text.pack . nodeName <$> getSystemID
                       ]
    maybeTags = sequence [ fmap (stringTag "ip" . formatSockAddr) <$> findSockAddr
                                       ]

getUname :: IO Text
getUname = do
    s <- getSystemID
    return $ Text.intercalate " " $ map Text.pack [ systemName s
                                                  , nodeName s
                                                  , release s
                                                  , version s
                                                  , machine s
                                                  ]

formatSockAddr :: SockAddr -> Text
formatSockAddr addr = Text.toStrict $ Text.toLazyText $ case addr of
    SockAddrInet _ h -> let (a1, a2, a3, a4) = hostAddressToTuple h in
        mconcat [ decimal a1, ".", decimal a2, ".", decimal a3, ".", decimal a4]
    SockAddrInet6 _ _ h _ -> let (a1, a2, a3, a4, a5, a6, a7, a8) = hostAddress6ToTuple h in
        mconcat [ hexadecimal a1
                , ":"
                , hexadecimal a2
                , ":"
                , hexadecimal a3
                , ":"
                , hexadecimal a4
                , ":"
                , hexadecimal a5
                , ":"
                , hexadecimal a6
                , ":"
                , hexadecimal a7
                , ":"
                , hexadecimal a8
                ]
    _ -> error "formatSockAddr: Unexpected SockAddr type"

findSockAddr :: IO (Maybe SockAddr)
findSockAddr = getInetInterfaces >>= \ifaddrs -> case sortBy cmp $ filter flt ifaddrs of
    [] -> pure Nothing
    (hd:_) -> pure (ifaAddr hd)
  where
    -- Filter out
    -- - Non-IP interfaces (though they shouldn't be listed in the first place)
    -- - Loopback interfaces
    -- - Down or non-running interfaces
    flt i = and [ case ifaAddr i of
                    Just SockAddrInet{} -> True
                    Just SockAddrInet6{} -> True
                    _ -> False
                , IFF_LOOPBACK `Set.notMember` ifaFlags i
                , IFF_UP `Set.member` ifaFlags i
                , IFF_RUNNING `Set.member` ifaFlags i
                ]
    -- Prefer IPv4 addresses to IPv6
    cmp a b = case (ifaAddr a, ifaAddr b) of
        (Just SockAddrInet{}, Just SockAddrInet{}) -> EQ
        (Just SockAddrInet{}, _) -> LT
        (_, Just SockAddrInet{}) -> GT
        _ -> EQ
