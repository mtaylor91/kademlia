module Kademlia.Core where

import Prelude hiding (length,toInteger)

import Control.Lens hiding (Context,index)
import qualified Data.ByteString as BS
import Data.Map (empty,insert)

import Kademlia.KID
import Kademlia.Types


kFactor :: Int
kFactor = kidBytes


newEmptyState :: NodeInfo a -> State a
newEmptyState node = updateBucket state [node] 255
  where state = State
          { kBuckets = take kidBits $ repeat []
          , localData = empty
          , localNode = node
          }


getState :: Context a -> IO (State a)
getState context = do
  let u = updateLocalState context
  ((), state) <- u (\s -> ((), s))
  return state


applyState :: Context a -> (State a -> State a) -> IO ()
applyState context f = do
  let u = updateLocalState context
  ((), _) <- u (\s -> ((), f s))
  return ()


insertData :: KID -> BS.ByteString -> State a -> State a
insertData kid value state =
  state { localData = insert kid value $ localData state }


isNode :: NodeInfo a -> NodeInfo b -> Bool
isNode n0 n1 = nodeID n0 == nodeID n1


isLocal :: Context a -> NodeInfo a -> Bool
isLocal context node = (nodeID node) == (nodeID . localNode . localState) context


sendNode :: Context a -> RPCRequest -> NodeInfo a -> IO (Maybe (RPCResult a))
sendNode context request node = (sendRPC context) (nodeAddr node) request


updateBucket :: State a -> [NodeInfo a] -> Int -> State a
updateBucket state bucket i =
  state { kBuckets = kBuckets state & element i .~ bucket }
