{-# LANGUAGE RankNTypes #-}
module Kademlia.Controller.Context where

import Kademlia.Controller.State        (State,Update,localNode)
import Kademlia.NodeInfo                (NodeInfo(..))
import Kademlia.RPC                     (RPCRequest,RPCResult,SendRPC)


data Context a = Context
  { sendRPC :: SendRPC a
  , localState :: State a
  , updateLocalState :: Update a
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


isLocal :: Context a -> NodeInfo a -> Bool
isLocal context node = (nodeID node) == (nodeID . localNode . localState) context


sendNode :: Context a -> RPCRequest -> NodeInfo a -> IO (Maybe (RPCResult a))
sendNode context request node = (sendRPC context) (nodeAddr node) request
