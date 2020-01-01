module UDP.Types where

import Data.Word (Word8)
import qualified Network.Socket as NS

import Types


type HostName = NS.HostName


type UDPPort = NS.ServiceName


type RequestType = Word8


type ResponseType = Word8


data UDPAddr = UDPAddr
  { hostname :: HostName
  , udpPort :: UDPPort
  } deriving (Eq,Show)


data UDPSocket = UDPSocket
  { socketAdvertiseAddress :: UDPAddr
  , socketAddActiveRequest :: KID -> (RPCResult UDPAddr -> IO ()) -> IO ()
  , socketGetActiveRequest :: KID -> IO (Maybe (RPCResult UDPAddr -> IO ()))
  , socketUDPSocket :: NS.Socket
  , socketKID :: KID
  }


data UDPMessage = UDPMessage
  { udpRequestKID :: KID
  , udpSourceKID :: KID
  , udpSourceAddr :: UDPAddr
  , udpMessageData :: UDPMessageData
  } deriving (Eq,Show)


data UDPMessageData
  = UDPRequest RPCRequest
  | UDPResponse (RPCResponse UDPAddr)
  deriving (Eq,Show)
