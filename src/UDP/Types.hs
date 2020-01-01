module UDP.Types where

import Data.Word (Word8)
import qualified Network.Socket as NS

import Types


type HostName = NS.HostName


type UDPPort = NS.ServiceName


type RequestType = Word8


type ResponseType = Word8


type RespondUDP = RPCResult UDPAddr -> IO ()


data UDPAddr = UDPAddr
  { hostname :: HostName
  , udpPort :: UDPPort
  } deriving (Eq)


data UDPSocket = UDPSocket
  { socketAdvertiseAddress :: UDPAddr
  , socketAddActiveRequest :: KID -> RespondUDP -> IO ()
  , socketGetActiveRequest :: KID -> IO (Maybe RespondUDP)
  , socketDeleteActiveRequest :: KID -> IO ()
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


instance Show UDPAddr where
  show (UDPAddr host port) = "udp://" <> host <> ":" <> port
