module Kademlia.UDP.Send where

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSBS
import System.Timeout

import Kademlia.Core (randomKID)
import Kademlia.Types (SendRPC)
import Kademlia.UDP.Core
import Kademlia.UDP.Encoding
import Kademlia.UDP.Types


responseTimeoutMilliseconds :: Int
responseTimeoutMilliseconds = 3000


send :: UDPSocket -> SendRPC UDPAddr
send s addr request = do
  requestKID <- randomKID

  let request' =
        LBS.toStrict $ encode $ UDPMessage
          { udpRequestKID = requestKID
          , udpSourceKID = socketKID s
          , udpSourceAddr = socketAdvertiseAddress s
          , udpMessageData = UDPRequest request
          }

  addr' <- getSocketAddr addr

  NSBS.sendAllTo (socketUDPSocket s) request' $ NS.addrAddress addr'

  mvar <- newEmptyMVar

  let respond = putMVar mvar

  (socketAddActiveRequest s) requestKID respond
  result <- timeout (1000 * responseTimeoutMilliseconds) $ takeMVar mvar
  (socketDeleteActiveRequest s) requestKID
  return result
