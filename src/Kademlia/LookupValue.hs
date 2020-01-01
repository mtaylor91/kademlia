module Kademlia.LookupValue (run) where

import Prelude hiding (lookup,until)

import Data.ByteString (ByteString)
import Data.List (nubBy,sortOn)
import Data.Map (lookup)

import Kademlia.Core
import Kademlia.BucketRefresh (refreshAll)
import Kademlia.ParallelProducer (until)
import Kademlia.Routing (findNearestNodes)
import Kademlia.Types


data LookupResults a = LookupResults
  { lookupTarget :: KID
  , lookupNearest :: [NodeInfo a]
  , lookupResults :: [(NodeInfo a, Maybe (RPCResponse a))]
  , lookupOutput :: Maybe ByteString
  }


instance Semigroup (LookupResults a) where
  LookupResults t n r o <> LookupResults _ n' r' o' = LookupResults
    { lookupTarget = t
    , lookupNearest = n''
    , lookupResults = r <> r'
    , lookupOutput = o''
    } where
      n'' =
        take kFactor $ sortOn (xor t . nodeKID . nodeID) $
        nubBy isNode $ sortOn nodeID $ n <> n'
      o'' = case (o, o') of
              (Just _, _) -> o
              (_, Just _) -> o'
              (Nothing, Nothing) -> Nothing


instance Monoid (LookupResults a) where
  mempty = LookupResults emptyKID [] [] Nothing
  mappend lr lr' = lr <> lr'


run :: Eq a => Context a -> KID -> IO (Maybe ByteString)
run context kid = do
  state <- getState context
  case lookup kid (localData state) of
    Just value ->
      return $ Just value
    Nothing -> do
      let nearest = findNearestNodes state kid kFactor
      results <- runLookup context kid nearest
      let seen = [ n | (n, Just _) <- lookupResults results ]
          misses = [ n | (n, Just (FoundNodes _)) <- lookupResults results ]
          nearMiss = take 1 $ sortOn (xor kid . nodeKID . nodeID) misses
          output = lookupOutput results
      refreshAll context seen
      case (output, nearMiss) of
        (Just value, [node]) -> do
          _ <- sendNode context (Store kid value) node
          return ()
        (_, _) ->
          return ()
      return $ lookupOutput results


runLookup :: Eq a => Context a -> KID -> [NodeInfo a] -> IO (LookupResults a)
runLookup context kid nodes =
  until lookupFinished (performLookup context kid) kFactor nodes


lookupFinished :: Eq a => LookupResults a -> LookupResults a -> Bool
lookupFinished lr lr' =
  case lookupOutput lr' of
    Nothing -> (lookupNearest lr) /= (lookupNearest lr')
    Just _ -> True


performLookup :: Eq a =>
  Context a -> KID -> NodeInfo a -> IO ([NodeInfo a], LookupResults a)
performLookup context kid node = do
  result <- sendNode context (FindValue kid) node
  case result of
    Just (node', FoundValue value) ->
      let r = Just $ FoundValue value
          results = LookupResults kid [] [(node', r)] (Just value)
       in return ([], results)
    Just (node', FoundNodes nodes) ->
      let r = Just $ FoundNodes nodes
          results = LookupResults kid nodes [(node', r)] Nothing
       in return (nodes, results)
    _ ->
      let results = LookupResults kid [] [(node, Nothing)] Nothing
       in return ([], results)
