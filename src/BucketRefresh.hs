module BucketRefresh (bucketRefresh,refreshAll) where

import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens hiding (Context)
import Data.List
import Data.List.Extra

import Core
import Routing  (getBucketIndex)
import Types


refreshAll :: NodeID -> Context a -> [NodeInfo a] -> IO ()
refreshAll nid context nodes = do
  let bi n = getBucketIndex nid $ (nodeKID . nodeID) n
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
  let liveNodes = [ n | (n, Just Pong) <- results ]
      deadNodes = [ n | (n, Nothing) <- results ]
  return (liveNodes, deadNodes)


updateNodes :: Int -> [NodeInfo a] -> State a -> ([NodeInfo a], State a)
updateNodes bi ns s =
  foldl u ([], s) ns
  where
    u (skipped, state) n =
      let bs = kBuckets state
          b = bs !! bi
          f n' = nodeID n == nodeID n'
       in case (findIndex f b, length b) of
            (Just i, _) ->
              let b' = b & element i .~ n
               in (skipped, updateBucket state b' bi)
            (Nothing, l) | l < kFactor ->
              let b' = b ++ [n]
               in (skipped, updateBucket state b' bi)
            (Nothing, _) ->
              (n:skipped, state)


removeNodes :: Int -> [NodeInfo a] -> State a -> ([NodeInfo a], State a)
removeNodes bi nodes state =
  let bs = kBuckets state
      b = bs !! bi
      f n = not $ any (isNode n) nodes
      b' = filter f b
   in (b', state { kBuckets = bs & element bi .~ b' })
