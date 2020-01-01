{-# LANGUAGE ScopedTypeVariables #-}
module Bootstrap (run) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Routing (getBucketIndex)
import Types


retryWaitSeconds :: Int
retryWaitSeconds = 3


run :: Show a => State a -> SendRPC a -> Maybe a -> IO (State a)
run state send maybePeerAddress =
  case maybePeerAddress of
    Just peerAddress ->
      ping state send peerAddress
    Nothing ->
      return state


ping :: Show a => State a -> SendRPC a -> a -> IO (State a)
ping state send peerAddress = do
  result <- send peerAddress Ping
  case result of
    Just (peer, Pong) ->
      let peerKID = nodeKID $ nodeID peer
          peerBucketIndex = getBucketIndex localID peerKID
          b = peer : ( kBuckets state !! peerBucketIndex )
       in return $ state
         { kBuckets = kBuckets state & element peerBucketIndex .~ b }
    _ -> do
      putStrLn $ "Unable to join: no response from " <> show peerAddress
      threadDelay $ retryWaitSeconds * 1000000
      ping state send peerAddress
  where
    localID = nodeID $ localNode state
