module Kademlia.Controller (start) where

import Prelude hiding (lookup)

import Control.Concurrent

import Kademlia.API
import Kademlia.Controller.Context
import Kademlia.Controller.Messages
import Kademlia.Controller.Processor
import Kademlia.Controller.State
import Kademlia.KID
import Kademlia.MemoryCache
import Kademlia.NodeInfo
import Kademlia.RPC

import qualified Kademlia.Tasks.Bootstrap as Bootstrap


start :: (Eq a, Show a) =>
  KID -> ProtocolBuilder a -> a -> Maybe a -> IO (API a)
start localID protocol localAddr maybePeerAddr = do
  Protocol _ send receive <- protocol localAddr localID

  cache <- memoryCache
  let node = NodeInfo localID localAddr
      state = newEmptyState node cache

  messages <- newEmptyMVar

  _ <- forkIO $ receiveLoop messages receive
  _ <- forkIO $ processLoop messages send state

  Bootstrap.run (Context send state $ update messages) maybePeerAddr

  return $ api messages


api :: MVar (Message a) -> API a
api messages = API
  { lookupNode = \kid ->
    apiRequest messages (LookupNode kid) $ \(LookupNodeResult r) -> r
  , lookupValue = \kid ->
    apiRequest messages (LookupValue kid) $ \(LookupValueResult r) -> r
  , insertValue = \kid bytes ->
    apiRequest messages (InsertValue kid bytes) $ \(InsertValueResult r) -> r
  }
