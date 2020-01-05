module Kademlia.Tasks.InsertValue (run) where

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString (ByteString)
import Data.List (filter)

import Kademlia.Controller.Context (Context,applyState,isLocal,localState,sendNode)
import Kademlia.Controller.State (insertData,localNode)
import Kademlia.KID (KID)
import Kademlia.NodeInfo (NodeInfo)
import Kademlia.RPC (RPCRequest(Store),RPCResponse(Stored))
import qualified Kademlia.Tasks.LookupNode as LookupNode


run :: Eq a => Context a -> KID -> ByteString -> IO [NodeInfo a]
run context kid value = do
  applyState context $ insertData kid value
  nearest <- LookupNode.run context kid
  let local = (localNode . localState $ context, Stored kid)
      remote = filter (not . isLocal context) nearest
  remoteResults <- mapConcurrently (sendNode context (Store kid value)) remote
  let results = Just local : remoteResults
  return [ node | Just (node, Stored _) <- results ]
