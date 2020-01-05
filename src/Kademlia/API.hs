module Kademlia.API where

import qualified Data.ByteString as BS

import Kademlia.KID             (KID)
import Kademlia.NodeInfo        (NodeInfo)


data API a = API
  { lookupNode :: KID -> IO [NodeInfo a]
  , lookupValue :: KID -> IO (Maybe BS.ByteString)
  , insertValue :: KID -> BS.ByteString -> IO [NodeInfo a]
  }


data APIRequest
  = LookupNode KID
  | LookupValue KID
  | InsertValue KID BS.ByteString
  deriving (Eq,Show)


data APIResponse a
  = LookupNodeResult [NodeInfo a]
  | LookupValueResult (Maybe BS.ByteString)
  | InsertValueResult [NodeInfo a]
  deriving (Eq,Show)


type APIRespond a = APIResponse a -> IO ()
