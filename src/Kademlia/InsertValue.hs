module Kademlia.InsertValue (run) where

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString (ByteString)
import Data.List (filter)

import Kademlia.Core (applyState,insertData,isLocal,sendNode)
import Kademlia.Types
import qualified Kademlia.LookupNode as LookupNode


run :: Eq a => Context a -> KID -> ByteString -> IO [NodeInfo a]
run context kid value = do
  applyState context $ insertData kid value
  nearest <- LookupNode.run context kid
  let local = (localNode . localState $ context, Stored kid)
      remote = filter (not . isLocal context) nearest
  remoteResults <- mapConcurrently (sendNode context (Store kid value)) remote
  let results = Just local : remoteResults
  return [ node | Just (node, Stored _) <- results ]
