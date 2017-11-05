{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Ifaddrs (
      Ifaddrs(..)
    , IFF(..)
    , getInetInterfaces
    ) where

#include <ifaddrs.h>
#include <net/if.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>

import Data.Bits ((.&.))
import Data.Int (Int32)
import Data.Word (Word16, Word32)

import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.String (peekCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, peekByteOff)

import Network.Socket (SockAddr)
import Network.Socket.Internal (peekSockAddr)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Exception.Safe (bracket)

data Ifaddrs = Ifaddrs { ifaName :: String
                       , ifaFlags :: Set IFF
                       , ifaAddr :: Maybe SockAddr
                       , ifaNetmask :: Maybe SockAddr
                       }
    deriving (Show, Eq)

data IFF = IFF_UP
         | IFF_LOOPBACK
         | IFF_POINTOPOINT
         | IFF_RUNNING
         | IFF_NOARP
         | IFF_PROMISC
    deriving (Show, Eq, Ord, Enum, Bounded)

mapIFF :: IFF -> #{type unsigned int}
mapIFF iff = case iff of
    IFF_UP -> #{const IFF_UP}
    IFF_LOOPBACK -> #{const IFF_LOOPBACK}
    IFF_POINTOPOINT -> #{const IFF_POINTOPOINT}
    IFF_RUNNING -> #{const IFF_RUNNING}
    IFF_NOARP -> #{const IFF_NOARP}
    IFF_PROMISC -> #{const IFF_PROMISC}

foreign import ccall "ifaddrs.h getifaddrs"
    c_getifaddrs :: Ptr (Ptr Ifaddrs) -> IO #{type int}

foreign import ccall "ifaddrs.h freeifaddrs"
    c_freeifaddrs :: Ptr Ifaddrs -> IO ()

getInetInterfaces :: IO [Ifaddrs]
getInetInterfaces = bracket getifaddrs c_freeifaddrs (loop [])
  where
    getifaddrs = alloca $ \ptr -> do
        throwErrnoIfMinus1_ "getifaddrs" $ c_getifaddrs ptr
        peek ptr
    loop acc ptr
        | ptr == nullPtr = return acc
        | otherwise = do
            ifa_next <- #{peek struct ifaddrs, ifa_next} ptr
            ifa_addr <- #{peek struct ifaddrs, ifa_addr} ptr
            sa_family <- #{peek struct sockaddr, sa_family} ifa_addr :: IO #{type sa_family_t}
            if (sa_family == #{const AF_INET} || sa_family == #{const AF_INET6})
                then do
                    ifa <- Ifaddrs <$> (#{peek struct ifaddrs, ifa_name} ptr >>= peekCString)
                                   <*> (parseFlags `fmap` #{peek struct ifaddrs, ifa_flags} ptr)
                                   <*> (#{peek struct ifaddrs, ifa_addr} ptr >>= maybePeek peekSockAddr)
                                   <*> (#{peek struct ifaddrs, ifa_netmask} ptr >>= maybePeek peekSockAddr)
                    loop (ifa : acc) ifa_next
                else loop acc ifa_next

parseFlags :: #{type unsigned int} -> Set IFF
parseFlags flags = foldr (\iff set -> if flags .&. (mapIFF iff) /= 0 then Set.insert iff set else set) Set.empty [minBound..maxBound]
