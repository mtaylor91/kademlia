{-# LANGUAGE ExistentialQuantification #-}
module Kademlia.Controller.Messages where

import Control.Concurrent

import Kademlia.API
import Kademlia.Controller.State
import Kademlia.NodeInfo
import Kademlia.RPC


data Message a
  = APICall APIRequest (APIResponse a -> IO ())
  | PeerRPC (NodeInfo a) RPCRequest (RespondRPC a)
  | forall r. Update (UpdateFunction a r) ((r, State a) -> IO ())


apiRequest :: MVar (Message a) -> APIRequest -> (APIResponse a -> b) -> IO b
apiRequest messages request translate = do
  mvar <- newEmptyMVar
  let respond result = putMVar mvar result
  putMVar messages $ APICall request respond
  result <- takeMVar mvar
  return $ translate result


update :: MVar (Message a) -> UpdateFunction a r -> IO (r, State a)
update messages u = do
  resultMVar <- newEmptyMVar
  putMVar messages $ Update u $ putMVar resultMVar
  takeMVar resultMVar


receiveLoop :: MVar (Message a) -> ReceiveRPC a -> IO ()
receiveLoop messages receive = do
  (nodestate, request, respond) <- receive
  putMVar messages $ PeerRPC nodestate request respond
  receiveLoop messages receive
