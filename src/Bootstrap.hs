module Bootstrap (run) where

import Control.Lens
import Routing (getBucketIndex)
import Types


run :: State a -> SendRPC a -> Maybe a -> IO (State a)
run state send maybePeerAddress =
  case maybePeerAddress of
    Just peerAddress -> do
      (peer, Just Pong) <- send peerAddress Ping
      let peerKID = nodeKID $ nodeID peer
          peerBucketIndex = getBucketIndex localID peerKID
          b = peer : ( kBuckets state !! peerBucketIndex )
       in return $ state
         { kBuckets = kBuckets state & element peerBucketIndex .~ b }
    Nothing ->
      return state
  where
    localID = nodeID $ localNode state
