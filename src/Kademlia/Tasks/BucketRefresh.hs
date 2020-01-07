module Kademlia.Tasks.BucketRefresh (run) where

import Data.List (nub,sort)

import Kademlia.Controller.Context (Context(..))
import Kademlia.Controller.State (localNode)
import Kademlia.KID (getBucketIndex,randomKIDInBucket)
import Kademlia.NodeInfo (NodeInfo(..))
import Kademlia.ParallelProducer (exhaustively)
import qualified Kademlia.Tasks.LookupNode as LookupNode


data BucketRefresh a = BucketRefresh [(Int, [NodeInfo a])]


instance Semigroup (BucketRefresh a) where
  BucketRefresh left <> BucketRefresh right =
    BucketRefresh $ left <> right


instance Monoid (BucketRefresh a) where
  mempty = BucketRefresh []
  mappend = (<>)


run :: Eq a => Context a -> IO (BucketRefresh a)
run context = do
  let parallelism = 32
  exhaustively (refreshBucket context) parallelism [255]


refreshBucket :: Eq a => Context a -> Int -> IO ([Int], BucketRefresh a)
refreshBucket context bucketIndex = do
  putStrLn $ "Refreshing bucket " <> show bucketIndex
  let localID = nodeID . localNode $ localState context
  randomBucketKID <- randomKIDInBucket localID bucketIndex
  nearestNodes <- LookupNode.run context randomBucketKID
  let indices = [ getBucketIndex localID $ nodeID n | n <- nearestNodes ]
      indices' = nub $ sort [ i | i <- indices, i < bucketIndex ]
  return (indices', BucketRefresh [(bucketIndex, nearestNodes)])
