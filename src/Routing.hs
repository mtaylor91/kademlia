{-# LANGUAGE ScopedTypeVariables #-}
module Routing where

import Data.Bits (testBit)
import Data.ByteArray (index)
import Data.List

import Core
import Types


getBucketIndex :: NodeID -> KID -> Int
getBucketIndex (NodeID nodekid) kid =
  firstNonZeroIndex [startIndex..endIndex]
    where
      startIndex :: Int = 0
      endIndex :: Int = kidBits - 1
      distance :: KID = xor kid nodekid
      firstNonZeroIndex x =
        case x of
          [] ->
            endIndex
          (i:indices') ->
            let w8 = index distance $ div i 8
            in if not $ testBit w8 $ 7 - rem i 8
                  then firstNonZeroIndex indices'
                  else i


findNearestNodes :: State a -> KID -> Int -> [NodeInfo a]
findNearestNodes state kid maxResults =
  f 1 $ bs !! i
  where
    bs = kBuckets state
    i = getBucketIndex (nodeID . localNode $ state) kid
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
      take maxResults $ sortOn (xor kid . nodeKID) results
