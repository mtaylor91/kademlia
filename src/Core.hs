{-# LANGUAGE ScopedTypeVariables #-}
module Core where

import Prelude hiding (length,toInteger)

import Basement.Block   (create,createFromPtr)
import Basement.Types.OffsetSize (CountOf(..))
import Control.Lens hiding (Context,index)
import Crypto.Hash      (Digest,SHA256,hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Map         (empty,insert)
import Data.Word        (Word8)
import System.Random    (getStdGen,setStdGen,random)

import Types


kFactor :: Int
kFactor = 32


kidBits :: Int
kidBits = kidBytes * 8


kidBytes :: Int
kidBytes = kFactor


emptyKID :: KID
emptyKID = KID $ create (CountOf kidBytes) (\_ -> 0 :: Word8)


createKID :: BS.ByteString -> Maybe KID
createKID bytes = if BS.length bytes < kidBytes then Nothing else
  Just $ KID $ BA.convert $ BS.take kidBytes bytes


randomKID :: IO KID
randomKID = do
  gen <- getStdGen
  let (bytes, gen') = foldl genRandom ([], gen) $ take kidBytes $ repeat ()
  let packed :: BA.Bytes = BA.pack bytes
  b <- BA.withByteArray packed $ \p -> createFromPtr p $ CountOf kidBytes
  setStdGen gen'
  return $ KID b
    where
      genRandom (accum, gen) () =
        let (r :: Word8, gen') = random gen
         in (r:accum, gen')



sha256KID :: BS.ByteString -> IO KID
sha256KID input = do
  let d :: Digest SHA256 = hash input
  b <- BA.withByteArray d $ \p -> createFromPtr p $ CountOf kidBytes
  return $ KID b


newEmptyState :: NodeInfo a -> State a
newEmptyState node = updateBucket state [node] 255
  where state = State
          { kBuckets = take kidBits $ repeat []
          , localData = empty
          , localNode = node
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


insertData :: KID -> BS.ByteString -> State a -> State a
insertData kid value state =
  state { localData = insert kid value $ localData state }


isNode :: NodeInfo a -> NodeInfo b -> Bool
isNode n0 n1 = nodeID n0 == nodeID n1


nodeKID :: NodeInfo a -> KID
nodeKID (NodeInfo (NodeID k) _) = k


xor :: KID -> KID -> KID
xor k k' = BA.xor k k'


isLocal :: Context a -> NodeInfo a -> Bool
isLocal context node = (nodeID node) == (nodeID . localNode . localState) context


sendNode :: Context a -> RPCRequest -> NodeInfo a -> IO (RPCResult a)
sendNode context request node = (sendRPC context) (nodeAddr node) request


updateBucket :: State a -> [NodeInfo a] -> Int -> State a
updateBucket state bucket i =
  state { kBuckets = kBuckets state & element i .~ bucket }
