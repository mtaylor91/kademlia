{-# LANGUAGE ExistentialQuantification #-}
module Protocol (bootstrap,start) where

import Prelude hiding (lookup)

import Control.Concurrent       (MVar,newEmptyMVar,takeMVar,putMVar,forkIO)
import Data.Map                 (lookup)

import Core
import BucketRefresh            (bucketRefresh)
import qualified InsertValue
import qualified LookupNode
import qualified LookupValue
import Routing                  (getBucketIndex,findNearestNodes)
import Types


data Message a
  = APICall APIRequest (APIResponse a -> IO ())
  | PeerRPC (NodeInfo a) RPCRequest (RespondRPC a)
  | forall r. Update (UpdateFunction a r) ((r, State a) -> IO ())


bootstrap :: Eq a => ProtocolBuilder a -> a -> IO (API a)
bootstrap builder addr = do
  kid <- randomKID
  protocol <- builder addr kid
  start protocol kid


start :: Eq a => Protocol a -> KID -> IO (API a)
start (Protocol addr send receive) kid = do
  let state = newEmptyState addr kid
  messages <- newEmptyMVar
  {- TODO: kademlia bootstrapping -}
  _ <- forkIO $ processLoop messages send state
  _ <- forkIO $ receiveLoop messages receive
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


processLoop :: Eq a => MVar (Message a) -> (SendRPC a) -> (State a) -> IO ()
processLoop messages send state = do
  let context = Context send state $ update messages
  message <- takeMVar messages
  state' <- case message of
    APICall request respond ->
      processAPICall context state request respond
    PeerRPC node request respond -> do
      let bi = getBucketIndex (nodeID . localNode $ state) $ nodeKID node
      _ <- forkIO $ bucketRefresh context bi [node]
      (response, state') <- processRPC request state
      respond response
      return state'
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


processRPC :: RPCRequest -> State a -> IO (RPCResponse a, State a)
processRPC request state =
  case request of
    Ping ->
      return $ (Pong, state)
    FindNodes (NodeID kid) ->
      let nodes = findNearestNodes state kid kFactor
       in return $ (FoundNodes nodes, state)
    FindValue kid ->
      case lookup kid (localData state) of
        Just value ->
          return $ (FoundValue value, state)
        Nothing ->
          let nodes = findNearestNodes state kid kFactor
           in return $ (FoundNodes nodes, state)
    Store kid value ->
      let state' = insertData kid value state
       in return $ (Stored kid, state')


receiveLoop :: MVar (Message a) -> ReceiveRPC a -> IO ()
receiveLoop messages receive = do
  (nodestate, request, respond) <- receive
  putMVar messages $ PeerRPC nodestate request respond
  receiveLoop messages receive


update :: MVar (Message a) -> UpdateFunction a r -> IO (r, State a)
update messages u = do
  resultMVar <- newEmptyMVar
  putMVar messages $ Update u $ putMVar resultMVar
  takeMVar resultMVar
