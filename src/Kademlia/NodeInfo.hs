module Kademlia.NodeInfo where

import Kademlia.KID             (KID)


data NodeInfo a = NodeInfo
  { nodeID :: KID
  , nodeAddr :: a
  } deriving (Eq,Show)


isNode :: NodeInfo a -> NodeInfo b -> Bool
isNode n0 n1 = nodeID n0 == nodeID n1
