module UDP.Core where

import qualified Network.Socket as NS

import UDP.Types


maxMessageLength :: Int
maxMessageLength = 65535


requestTypePing :: RequestType
requestTypePing = 0


requestTypeFindNodes :: RequestType
requestTypeFindNodes = 1


requestTypeFindValue :: RequestType
requestTypeFindValue = 2


requestTypeStore :: RequestType
requestTypeStore = 3


responseTypePong :: ResponseType
responseTypePong = 4


responseTypeFoundNodes :: ResponseType
responseTypeFoundNodes = 5


responseTypeFoundValue :: ResponseType
responseTypeFoundValue = 6


responseTypeStored :: ResponseType
responseTypeStored = 7


getSocketAddr :: UDPAddr -> IO NS.AddrInfo
getSocketAddr (UDPAddr host port) = do
  addr:_ <- NS.getAddrInfo (Just socketAddrHints) (Just host) (Just port)
  return addr


socketAddrHints :: NS.AddrInfo
socketAddrHints =
  NS.defaultHints
    { NS.addrFlags = [NS.AI_NUMERICSERV]
    , NS.addrSocketType = NS.Datagram
    }
