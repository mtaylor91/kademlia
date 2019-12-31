{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, LiberalTypeSynonyms #-}
module Types where

import Basement.Block   (Block)
import Data.ByteArray   (ByteArray,ByteArrayAccess)
import Data.ByteString  (ByteString)
import Data.Map         (Map)
import Data.Word        (Word8)


type SendRPC a = a -> RPCRequest -> IO (RPCResult a)


type RespondRPC a = RPCResponse a -> IO ()


type RPCResult a = (NodeInfo a, Maybe (RPCResponse a))


type ReceiveRPC a = IO (NodeInfo a, RPCRequest, RespondRPC a)


type UpdateFunction a r = State a -> (r, State a)


type Update a = forall r. UpdateFunction a r -> IO (r, State a)


type ProtocolBuilder a = a -> KID -> IO (Protocol a)


type APIRespond a = APIResponse a -> IO ()


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


data Message a
  = APICall APIRequest (APIResponse a -> IO ())
  | PeerRPC (NodeInfo a) RPCRequest (RespondRPC a)
  | forall r. Update (UpdateFunction a r) ((r, State a) -> IO ())


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
  deriving (Show)


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


newtype KID = KID (Block Word8)
  deriving (Eq,Ord,Monoid,Semigroup,ByteArray,ByteArrayAccess,Show)


newtype NodeID = NodeID KID deriving (Eq,Ord,Show)
