{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes, LiberalTypeSynonyms #-}
module Types where

import Basement.Block   (Block)
import Data.ByteArray   (ByteArray,ByteArrayAccess)
import Data.ByteString  (ByteString)
import Data.Map         (Map)
import Data.Word        (Word8)


type Send a = a -> Request -> IO (Result a)


type Respond a = Response a -> IO ()


type Results a = [Result a]


type Result a = (NodeInfo a, Maybe (Response a))


type Receive a = IO (NodeInfo a, Request, Respond a)


type UpdateFunction a r = State a -> (r, State a)


type Update a = forall r. UpdateFunction a r -> IO (r, State a)


type KBuckets a = [[NodeInfo a]]


data Protocol a
  = Protocol a (Send a) (Receive a)


data Message a
  = LookupNode KID ([NodeInfo a] -> IO ())
  | LookupValue KID (Maybe ByteString -> IO ())
  | InsertValue KID ByteString ([NodeInfo a] -> IO ())
  | PeerRPC (NodeInfo a) Request (Respond a)
  | forall r. Update (UpdateFunction a r) ((r, State a) -> IO ())


data Request
  = Ping
  | FindNodes NodeID
  | FindValue KID
  | Store KID ByteString
  deriving (Eq,Show)


data Response a
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
  { kBuckets :: KBuckets a
  , localData :: Map KID ByteString
  , localNodeID :: NodeID
  , localNodeAddr :: a
  } deriving (Show)


data Context a = Context
  { sendRPC :: Send a
  , localState :: State a
  , updateLocalState :: Update a
  }


newtype KID = KID (Block Word8)
  deriving (Eq,Ord,Monoid,Semigroup,ByteArray,ByteArrayAccess,Show)


newtype NodeID = NodeID KID deriving (Eq,Ord,Show)
