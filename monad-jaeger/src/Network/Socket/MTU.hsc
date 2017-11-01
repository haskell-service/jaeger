module Network.Socket.MTU (
      getMTU
    ) where

#include <netinet/in.h>

import Network.Socket (Socket, SocketOption(CustomSockOpt), getSocketOption)

-- | Get the MTU of a connected socket, as determined by the kernel.
--
-- Basically, `getsockopt(IPPROTO_IP, IP_MTU)`.
--
-- If not supported on the platform, return 1500.
getMTU :: Socket -> IO Word
#if defined(IPPROTO_IP) && defined(IP_MTU)
getMTU sock = fromIntegral <$> getSocketOption sock iP_MTU
  where
    iP_MTU = CustomSockOpt (#{const IPPROTO_IP}, #{const IP_MTU})
#else
getMTU _ = pure 1500
#endif
