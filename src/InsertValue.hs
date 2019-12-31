module InsertValue (run) where

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString (ByteString)
import Data.List (partition)

import Core (applyState,insertData,isLocal,sendNode)
import qualified LookupNode
import Types


run :: Eq a => Context a -> KID -> ByteString -> IO [NodeInfo a]
run context kid value = do
  nearest <- LookupNode.run context kid
  let (local, remote) = partition (isLocal context) nearest
  localResults <- case local of
    localNode:_ -> do
      applyState context $ insertData kid value
      return [(localNode, Just (Stored kid))]
    [] ->
      return []
  remoteResults <- mapConcurrently (sendNode context (Store kid value)) remote
  let results = localResults ++ remoteResults
  return [ node | (node, Just (Stored _)) <- results ]
