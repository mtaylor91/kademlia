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
  , socketAddActiveRequest :: KID -> (Result UDPAddr -> IO ()) -> IO ()
  , socketGetActiveRequest :: KID -> IO (Maybe (Result UDPAddr -> IO ()))
  , socketUDPSocket :: NS.Socket
  , socketKID :: KID
  }


data UDPMessage = UDPMessage
  { udpRequestKID :: KID
  , udpSourceKID :: KID
  , udpSourceAddr :: UDPAddr
  , udpMessageData :: UDPMessageData
  } deriving (Show)


data UDPMessageData
  = UDPRequest Request
  | UDPResponse (Response UDPAddr)
  deriving (Show)
