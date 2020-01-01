{-# LANGUAGE ExistentialQuantification #-}
module Kademlia.Controller (bootstrap,start) where

import Prelude hiding (lookup)

import Control.Concurrent       (MVar,newEmptyMVar,takeMVar,putMVar,forkIO)
import Data.Map                 (lookup)

import Kademlia.Core
import Kademlia.BucketUpdate             (bucketUpdate)
import Kademlia.KID
import Kademlia.Routing                  (getBucketIndex,findNearestNodes)
import Kademlia.Types

import qualified Kademlia.Bootstrap as Bootstrap
import qualified Kademlia.InsertValue as InsertValue
import qualified Kademlia.LookupNode as LookupNode
import qualified Kademlia.LookupValue as LookupValue


data Message a
  = APICall APIRequest (APIResponse a -> IO ())
  | PeerRPC (NodeInfo a) RPCRequest (RespondRPC a)
  | forall r. Update (UpdateFunction a r) ((r, State a) -> IO ())


bootstrap :: (Eq a, Show a) => ProtocolBuilder a -> a -> Maybe a -> IO (API a)
bootstrap builder localAddr maybePeerAddr = do
  kid <- randomKID
  protocol <- builder localAddr kid
  start protocol kid maybePeerAddr


start :: (Eq a, Show a) => Protocol a -> KID -> Maybe a -> IO (API a)
start (Protocol addr send receive) kid maybePeerAddr = do
  let node = NodeInfo (NodeID kid) addr
      state = newEmptyState node

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


processLoop :: Eq a => MVar (Message a) -> (SendRPC a) -> (State a) -> IO ()
processLoop messages send state = do
  let context = Context send state $ update messages
  message <- takeMVar messages
  state' <- case message of
    APICall request respond ->
      processAPICall context state request respond
    PeerRPC node request respond -> do
      processRPC context state node request respond
    Update u respond -> do
      let (r, state') = u state
      respond (r, state')
      return state'
  processLoop messages send state'


processAPICall :: Eq a =>
  Context a -> State a -> APIRequest -> APIRespond a -> IO (State a)
processAPICall context state request respond = do
  case request of
    LookupNode kid -> do
      _ <- forkIO $ do
        nodes <- LookupNode.run context kid
        respond $ LookupNodeResult nodes
      return state
    LookupValue kid -> do
      _ <- forkIO $ do
        value <- LookupValue.run context kid
        respond $ LookupValueResult value
      return state
    InsertValue kid value -> do
      _ <- forkIO $ do
        nodes <- InsertValue.run context kid value
        respond $ InsertValueResult nodes
      return state


processRPC ::
  Context a -> State a -> NodeInfo a -> RPCRequest -> RespondRPC a ->
    IO (State a)
processRPC context state node request respond = do
  {- Refresh sender's kBucket -}
  let local = localNode state
      bucketIndes = getBucketIndex (nodeID local) $ (nodeKID . nodeID) node
  _ <- forkIO $ bucketUpdate context bucketIndes [node]
  {- Handle RPC request -}
  case request of
    Ping -> do
      respond Pong
      return state
    FindNodes (NodeID kid) -> do
      respond $ FoundNodes $ findNearestNodes state kid kFactor
      return state
    FindValue kid ->
      case lookup kid (localData state) of
        Just value -> do
          respond $ FoundValue value
          return state
        Nothing -> do
          respond $ FoundNodes $ findNearestNodes state kid kFactor
          return state
    Store kid value -> do
      respond $ Stored kid
      return $ insertData kid value state
