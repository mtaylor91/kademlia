module UDP.Send where

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSBS

import Core (randomKID)
import Types (Send)
import UDP.Core
import UDP.Encoding
import UDP.Types


send :: UDPSocket -> Send UDPAddr
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

  takeMVar mvar {- TODO: timeout / cleanup -}
