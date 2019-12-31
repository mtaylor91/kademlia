{-# LANGUAGE RankNTypes #-}
module LookupNode (lookupNode) where

import Prelude hiding (until)

import Data.List

import Core
import BucketRefresh (refreshAll)
import Parallel (until)
import Routing (findNearestNodes)
import Types hiding (LookupNode)


data LookupResults a = LookupResults
  { lookupTarget :: KID
  , lookupNearest :: [NodeInfo a]
  , lookupResults :: [(NodeInfo a, Maybe [NodeInfo a])]
  }


instance Semigroup (LookupResults a) where
  LookupResults t n r <> LookupResults _ n' r' = LookupResults
    { lookupTarget = t
    , lookupNearest = n''
    , lookupResults = r <> r'
    } where
      n'' =
        take kFactor $ sortOn (xor t . nodeKID) $
        nubBy isNode $ sortOn nodeID $ n <> n'


instance Monoid (LookupResults a) where
  mempty = LookupResults emptyKID [] []
  mappend lr lr' = lr <> lr'


lookupNode :: Eq a => Context a -> KID -> IO [NodeInfo a]
lookupNode context kid = do
  state <- getState context
  let nodes = findNearestNodes state kid kFactor
  results <- runLookup context kid nodes
  let seen = [ n | (n, Just _) <- lookupResults results ]
      nodes' = lookupNearest results
  refreshAll (localNodeID state) context seen
  return nodes'


runLookup :: Eq a => Context a -> KID -> [NodeInfo a] -> IO (LookupResults a)
runLookup context kid nodes =
  until lookupFinished (performLookup context kid) kFactor nodes


lookupFinished :: Eq a => LookupResults a -> LookupResults a -> Bool
lookupFinished lr lr' = (lookupNearest lr) /= (lookupNearest lr')


performLookup :: Eq a =>
  Context a -> KID -> NodeInfo a -> IO ([NodeInfo a], LookupResults a)
performLookup context kid node = do
  result <- sendNode context (FindNodes $ NodeID kid) node
  case result of
    (node', Just (FoundNodes nodes)) ->
      return (nodes, LookupResults kid nodes [(node', Just nodes)])
    (node', _) ->
      return ([], LookupResults kid [] [(node', Nothing)])
