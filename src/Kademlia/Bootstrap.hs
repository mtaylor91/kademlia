{-# LANGUAGE ScopedTypeVariables #-}
module Kademlia.Bootstrap (run) where

import Control.Concurrent (threadDelay)

import Kademlia.BucketRefresh (bucketRefresh)
import Kademlia.Routing (getBucketIndex)
import Kademlia.Types

import qualified Kademlia.LookupNode as LookupNode


retryWaitSeconds :: Int
retryWaitSeconds = 3


run :: (Eq a, Show a) => Context a -> Maybe a -> IO ()
run context maybePeerAddress =
  case maybePeerAddress of
    Just peerAddress ->
      ping context peerAddress
    Nothing ->
      return ()


ping :: (Eq a, Show a) => Context a -> a -> IO ()
ping context peerAddress = do
  result <- (sendRPC context) peerAddress Ping
  case result of
    Just (peer, Pong) -> do
      let localID = nodeID $ localNode $ localState context
          peerBucketIndex = getBucketIndex localID peerKID
          peerKID = nodeKID $ nodeID peer
      bucketRefresh context peerBucketIndex [peer]
      bootstrap context
    _ -> do
      putStrLn $ "Unable to join: no response from " <> show peerAddress
      threadDelay $ retryWaitSeconds * 1000000
      ping context peerAddress


bootstrap :: Eq a => Context a -> IO ()
bootstrap context = do
  let kid = nodeKID $ nodeID $ localNode $ localState context
  _ <- LookupNode.run context kid
  return ()
