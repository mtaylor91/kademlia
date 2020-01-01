{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, LiberalTypeSynonyms #-}
module Types where

import Basement.Types.Word256   (Word256)
import Data.ByteString          (ByteString)
import Data.Map                 (Map)


newtype KID = KID Word256
  deriving (Eq,Ord,Show)


newtype NodeID = NodeID { nodeKID :: KID } deriving (Eq,Ord,Show)


data API a = API
  { lookupNode :: KID -> IO [NodeInfo a]
  , lookupValue :: KID -> IO (Maybe ByteString)
  , insertValue :: KID -> ByteString -> IO [NodeInfo a]
  }


data APIRequest
  = LookupNode KID
  | LookupValue KID
  | InsertValue KID ByteString
  deriving (Eq,Show)


data APIResponse a
  = LookupNodeResult [NodeInfo a]
  | LookupValueResult (Maybe ByteString)
  | InsertValueResult [NodeInfo a]
  deriving (Eq,Show)


data Protocol a
  = Protocol a (SendRPC a) (ReceiveRPC a)


data RPCRequest
  = Ping
  | FindNodes NodeID
  | FindValue KID
  | Store KID ByteString
  deriving (Eq,Show)


data RPCResponse a
  = Pong
  | FoundNodes [NodeInfo a]
  | FoundValue ByteString
  | Stored KID
  deriving (Eq,Show)


data NodeInfo a = NodeInfo
  { nodeID :: NodeID
  , nodeAddr :: a
  } deriving (Eq,Show)


data State a = State
  { kBuckets :: [[NodeInfo a]]
  , localData :: Map KID ByteString
  , localNode :: NodeInfo a
  } deriving (Show)


data Context a = Context
  { sendRPC :: SendRPC a
  , localState :: State a
  , updateLocalState :: Update a
  }


type SendRPC a = a -> RPCRequest -> IO (Maybe (RPCResult a))


type RespondRPC a = RPCResponse a -> IO ()


type RPCResult a = (NodeInfo a, RPCResponse a)


type ReceiveRPC a = IO (NodeInfo a, RPCRequest, RespondRPC a)


type UpdateFunction a r = State a -> (r, State a)


type Update a = forall r. UpdateFunction a r -> IO (r, State a)


type ProtocolBuilder a = a -> KID -> IO (Protocol a)


type APIRespond a = APIResponse a -> IO ()
