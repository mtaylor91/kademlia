module UDP.Receive where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSBS

import Types (NodeID(..),NodeInfo(..),ReceiveRPC,RPCRequest,RPCResponse)
import UDP.Core
import UDP.Decoding
import UDP.Encoding
import UDP.Types


receive :: UDPSocket -> ReceiveRPC UDPAddr
receive s = do
  (bytes, origin) <- NSBS.recvFrom (socketUDPSocket s) maxMessageLength
  case decode bytes of
    Just message ->
      case udpMessageData message of
        UDPRequest request ->
          receiveRequest s message request origin
        UDPResponse response -> do
          receiveResponse s message response
          receive s
    Nothing ->
      receive s


receiveRequest ::
  UDPSocket -> UDPMessage -> RPCRequest -> NS.SockAddr -> ReceiveRPC UDPAddr
receiveRequest s message request origin = do
  let sender = NodeInfo senderID senderAddr
      senderID = NodeID $ udpSourceKID message
      senderAddr = udpSourceAddr message
      respond response = do
        let response' =
              LBS.toStrict $ encode $ UDPMessage
                { udpRequestKID = udpRequestKID message
                , udpSourceKID = socketKID s
                , udpSourceAddr = socketAdvertiseAddress s
                , udpMessageData = UDPResponse response
                }
        NSBS.sendAllTo (socketUDPSocket s) response' origin
  return (sender, request, respond)


receiveResponse :: UDPSocket -> UDPMessage -> RPCResponse UDPAddr -> IO ()
receiveResponse s message response = do
  active <- (socketGetActiveRequest s) (udpRequestKID message)
  case active of
    Just respond -> do
      let node = NodeInfo
            (NodeID $ udpSourceKID message)
            (udpSourceAddr message)
      respond (node, response)
    Nothing ->
      return ()
