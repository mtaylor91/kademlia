module Kademlia.BucketRefresh (bucketRefresh,refreshAll) where

import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Data.List
import Data.List.Extra

import Kademlia.Core
import Kademlia.Routing
import Kademlia.Types


refreshAll :: Context a -> [NodeInfo a] -> IO ()
refreshAll context nodes = do
  let nid = nodeID $ localNode $ localState context
      bi n = getBucketIndex nid $ (nodeKID . nodeID) n
      r ns = forkIO $ bucketRefresh context (bi (head ns)) ns
  sequence_ $ fmap r $ groupOn bi $ sortOn bi $ nodes


bucketRefresh :: Context a -> Int -> [NodeInfo a] -> IO ()
bucketRefresh context bi nodes = do
  (skipped, state) <- (updateLocalState context) $ updateNodes bi nodes
  if length skipped == 0 then return () else do
    let bs = kBuckets state
        nodes' = bs !! bi
    (liveNodes, deadNodes) <- pingNodes context nodes'
    if length liveNodes >= length nodes' then return () else do
      _ <- (updateLocalState context) $ removeNodes bi deadNodes
      _ <- (updateLocalState context) $ updateNodes bi liveNodes
      return ()


pingNodes :: Context a -> [NodeInfo a] -> IO ([NodeInfo a], [NodeInfo a])
pingNodes context nodes = do
  let send = sendNode context
  results <- mapConcurrently (send Ping) nodes
  let liveNodes = [ n | Just (n, Pong) <- results ]
      deadNodes = [ n | n <- nodes, not $ any (isNode n) liveNodes ]
  return (liveNodes, deadNodes)
