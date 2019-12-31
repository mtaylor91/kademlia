module InsertValue (run) where

import Control.Concurrent.Async (mapConcurrently)
import Data.ByteString (ByteString)
import Data.List (filter)

import Core (applyState,insertData,isLocal,sendNode)
import qualified LookupNode
import Types


run :: Eq a => Context a -> KID -> ByteString -> IO [NodeInfo a]
run context kid value = do
  applyState context $ insertData kid value
  nearest <- LookupNode.run context kid
  let local = ((localNode . localState $ context), Just (Stored kid))
      remote = filter (not . isLocal context) nearest
  remoteResults <- mapConcurrently (sendNode context (Store kid value)) remote
  let results = local : remoteResults
  return [ node | (node, Just (Stored _)) <- results ]
