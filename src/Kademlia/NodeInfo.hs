{-# LANGUAGE DeriveGeneric #-}
module Kademlia.NodeInfo where

import Data.Aeson
import GHC.Generics

import Kademlia.KID (KID)


data NodeInfo a = NodeInfo
  { nodeID :: KID
  , nodeAddr :: a
  } deriving (Generic,Eq,Show)


instance (FromJSON a) => FromJSON (NodeInfo a) where


instance (ToJSON a) => ToJSON (NodeInfo a) where


isNode :: NodeInfo a -> NodeInfo b -> Bool
isNode n0 n1 = nodeID n0 == nodeID n1
