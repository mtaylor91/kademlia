{-# LANGUAGE ScopedTypeVariables #-}
module Routing where

import Prelude hiding (toInteger)

import Basement.Bits (countLeadingZeros)
import Basement.Numerical.Number (toInteger)
import Control.Lens
import Data.List

import Core
import Types


getBucketIndex :: NodeID -> KID -> Int
getBucketIndex (NodeID nodekid) kid =
  (fromInteger $ toInteger $ countLeadingZeros $ xor nodekid kid) - 1


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
      take maxResults $ sortOn (xor kid . nodeKID . nodeID) results


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
