{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Kademlia.Controller.State where

import Control.Lens
import Data.List                (findIndex,sortOn)
import qualified Data.ByteString as BS

import Kademlia.Cache           (Cache,query,store)
import Kademlia.KID             (KID,getBucketIndex,kidBits,kidBytes,xor)
import Kademlia.NodeInfo        (NodeInfo,isNode,nodeID)


data State a = forall c. Cache c => State
  { kBuckets :: [[NodeInfo a]]
  , localNode :: NodeInfo a
  , stateCache :: c
  }


type UpdateFunction a r = State a -> (r, State a)


type Update a = forall r. UpdateFunction a r -> IO (r, State a)


newEmptyState :: Cache c => NodeInfo a -> c -> State a
newEmptyState node cache = replaceBucket state [node] 255
  where state = State
          { kBuckets = take kidBits $ repeat []
          , localNode = node
          , stateCache = cache
          }


query :: State a -> KID -> IO (Maybe (IO BS.ByteString))
query (State _ _ cache) kid =
  Kademlia.Cache.query cache kid


store :: State a -> KID -> BS.ByteString -> IO ()
store (State _ _ cache) kid bytes =
  Kademlia.Cache.store cache kid bytes


replaceBucket :: State a -> [NodeInfo a] -> Int -> State a
replaceBucket state bucket i =
  state { kBuckets = kBuckets state & element i .~ bucket }


findNearestNodes :: State a -> KID -> Int -> [NodeInfo a]
findNearestNodes state kid maxResults =
  f 1 $ bs !! i
  where
    local = localNode state
    bs = kBuckets state
    i = getBucketIndex (nodeID $ local) kid
    f o accum =
      case (length accum, i+o) of
        (l, _) | l >= maxResults ->
          sortAndFilter accum
        (_, j) | j <= kidBits - 1 ->
          f (o+1) $ accum ++ bs !! j
        (_, _) ->
          g 1 accum
    g o accum =
      case (length accum, i-o) of
        (l, _) | l >= maxResults ->
          sortAndFilter accum
        (_, j) | j >= 0 ->
          g (o+1) $ accum ++ bs !! j
        (_, _) ->
          sortAndFilter accum
    sortAndFilter results =
      take maxResults $ sortOn (xor kid . nodeID) results


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
               in (skipped, replaceBucket state b' bi)
            (Nothing, l) | l < kidBytes ->
              let b' = b ++ [n]
               in (skipped, replaceBucket state b' bi)
            (Nothing, _) ->
              (n:skipped, state)


removeNodes :: Int -> [NodeInfo a] -> State a -> ([NodeInfo a], State a)
removeNodes bi nodes state =
  let bs = kBuckets state
      b = bs !! bi
      f n = not $ any (isNode n) nodes
      b' = filter f b
   in (b', state { kBuckets = bs & element bi .~ b' })
