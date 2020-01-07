module Kademlia.Tasks.InsertValue (run) where

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString (ByteString)
import Data.List (filter)

import Kademlia.Controller.Context (Context,isLocal,localState,sendNode,store)
import Kademlia.Controller.State (State(..),localNode)
import Kademlia.KID (KID)
import Kademlia.NodeInfo (NodeInfo)
import Kademlia.RPC (RPCRequest(Store),RPCResponse(Stored))
import qualified Kademlia.Tasks.LookupNode as LookupNode


run :: Eq a => Context a -> KID -> ByteString -> IO [NodeInfo a]
run context kid value = do
  store context kid value
  let localResult = (localNode . localState $ context, Stored kid)
  nearest <- LookupNode.run context kid
  let send = sendNode context (Store kid value)
      remoteNodes = filter (not . isLocal context) nearest
  remoteResults <- mapConcurrently send remoteNodes
  let results = Just localResult : remoteResults
  return [ node | Just (node, Stored _) <- results ]
