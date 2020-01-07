module Kademlia.Controller.Processor (processLoop) where

import Prelude hiding (lookup)

import Control.Concurrent

import Kademlia.API
import Kademlia.Controller.Context hiding (query,store)
import Kademlia.Controller.Messages
import Kademlia.Controller.State
import Kademlia.KID
import Kademlia.NodeInfo
import Kademlia.RPC
import Kademlia.Tasks.BucketUpdate

import qualified Kademlia.Tasks.InsertValue as InsertValue
import qualified Kademlia.Tasks.LookupNode as LookupNode
import qualified Kademlia.Tasks.LookupValue as LookupValue


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
      bucketIndex = getBucketIndex (nodeID local) $ nodeID node
  _ <- forkIO $ updateBucket context bucketIndex [node]
  {- Handle RPC request -}
  case request of
    Ping -> do
      respond Pong
      return state
    FindNodes kid -> do
      respond $ FoundNodes $ findNearestNodes state kid kidBytes
      return state
    FindValue kid -> do
      maybeValueIO <- query state kid
      case maybeValueIO of
        Just valueIO -> do
          value <- valueIO
          respond $ FoundValue value
          return state
        Nothing -> do
          respond $ FoundNodes $ findNearestNodes state kid kidBytes
          return state
    Store kid value -> do
      store state kid value
      respond $ Stored kid
      return $ state
