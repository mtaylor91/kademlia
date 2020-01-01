{-# LANGUAGE ScopedTypeVariables #-}
module Routing where

import Prelude hiding (toInteger)

import Basement.Bits (countLeadingZeros)
import Basement.Numerical.Number (toInteger)
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
