module Protocol (start) where

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


start :: Eq a => Protocol a -> KID -> IO ()
start (Protocol addr send receive) kid = do
  let state = newEmptyState addr kid
  messages <- newEmptyMVar
  {- TODO: kademlia bootstrapping -}
  _ <- forkIO $ processLoop messages send state
  _ <- forkIO $ receiveLoop messages receive
  return ()


processLoop :: Eq a => MVar (Message a) -> (SendRPC a) -> (State a) -> IO ()
processLoop messages send state = do
  let context = Context send state $ update messages
  message <- takeMVar messages
  state' <- case message of
    LookupNode kid respond -> do
      _ <- forkIO $ LookupNode.run context kid >>= respond
      return state
    LookupValue kid respond -> do
      _ <- forkIO $ LookupValue.run context kid >>= respond
      return state
    InsertValue kid value respond -> do
      _ <- forkIO $ InsertValue.run context kid value >>= respond
      return state
    PeerRPC node request respond -> do
      let bi = getBucketIndex (localNodeID state) $ nodeKID node
      _ <- forkIO $ bucketRefresh context bi [node]
      (response, state') <- processRPC request state
      respond response
      return state'
    Update u respond -> do
      let (r, state') = u state
      respond (r, state')
      return state'
  processLoop messages send state'


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
