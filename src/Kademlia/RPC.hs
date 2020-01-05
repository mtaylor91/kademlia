{-# LANGUAGE RankNTypes, LiberalTypeSynonyms #-}
module Kademlia.RPC where

import Data.ByteString          (ByteString)

import Kademlia.KID             (KID)
import Kademlia.NodeInfo        (NodeInfo)


data Protocol a
  = Protocol a (SendRPC a) (ReceiveRPC a)


data RPCRequest
  = Ping
  | FindNodes KID
  | FindValue KID
  | Store KID ByteString
  deriving (Eq,Show)


data RPCResponse a
  = Pong
  | FoundNodes [NodeInfo a]
  | FoundValue ByteString
  | Stored KID
  deriving (Eq,Show)


type SendRPC a = a -> RPCRequest -> IO (Maybe (RPCResult a))


type RespondRPC a = RPCResponse a -> IO ()


type RPCResult a = (NodeInfo a, RPCResponse a)


type ReceiveRPC a = IO (NodeInfo a, RPCRequest, RespondRPC a)


type ProtocolBuilder a = a -> KID -> IO (Protocol a)
