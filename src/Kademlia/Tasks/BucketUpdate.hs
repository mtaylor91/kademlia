module Kademlia.Tasks.BucketUpdate (updateBucket,updateBuckets) where

import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Data.List
import Data.List.Extra

import Kademlia.Controller.Context      (Context,localState,sendNode,updateLocalState)
import Kademlia.Controller.State        (kBuckets,localNode,removeNodes,updateNodes)
import Kademlia.KID                     (getBucketIndex)
import Kademlia.NodeInfo                (NodeInfo,isNode,nodeID)
import Kademlia.RPC                     (RPCRequest(Ping),RPCResponse(Pong))


updateBuckets :: Context a -> [NodeInfo a] -> IO ()
updateBuckets context nodes = do
  let nid = nodeID $ localNode $ localState context
      bi n = getBucketIndex nid $ nodeID n
      r ns = forkIO $ updateBucket context (bi (head ns)) ns
  sequence_ $ fmap r $ groupOn bi $ sortOn bi $ nodes


updateBucket :: Context a -> Int -> [NodeInfo a] -> IO ()
updateBucket context bi nodes = do
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
