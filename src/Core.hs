{-# LANGUAGE ScopedTypeVariables #-}
module Core where

import Prelude hiding (length,toInteger)

import Basement.Block (Block,cast,createFromPtr,index)
import Basement.Types.OffsetSize (CountOf(..),Offset(..))
import Basement.Types.Word256 (Word256,bitwiseXor)
import Control.Lens hiding (Context,index)
import Crypto.Hash (Digest,SHA256,hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Map (empty,insert)
import Data.Word (Word8)
import System.Random (getStdGen,setStdGen,random)

import Types


kFactor :: Int
kFactor = 32


kidBits :: Int
kidBits = kidBytes * 8


kidBytes :: Int
kidBytes = kFactor


emptyKID :: KID
emptyKID = KID 0


createKID :: BS.ByteString -> Maybe KID
createKID bytes = if BS.length bytes < kidBytes then Nothing else
  let bytes' :: Block Word8 = BA.convert $ BS.take kidBytes bytes
   in Just $ KID $ index (cast bytes') (Offset 0)


randomKID :: IO KID
randomKID = do
  gen <- getStdGen

  let (bytes, gen') = foldl genRandom ([], gen) $ take kidBytes $ repeat ()

  buffer <- BA.withByteArray (BA.pack bytes :: BA.Bytes) $ \p ->
    (createFromPtr p $ CountOf kidBytes) :: IO (Block Word256)

  setStdGen gen'

  return $ KID $ index buffer (Offset 0)

    where

      genRandom (accum, gen) () =
        let (r :: Word8, gen') = random gen
         in (r:accum, gen')



sha256KID :: BS.ByteString -> IO KID
sha256KID input = do
  let d :: Digest SHA256 = hash input
  buffer <- BA.withByteArray d $ \p ->
    (createFromPtr p $ CountOf kidBytes) :: IO (Block Word256)
  return $ KID $ index buffer (Offset 0)


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


xor :: KID -> KID -> Word256
xor (KID k) (KID k') = bitwiseXor k k'


isLocal :: Context a -> NodeInfo a -> Bool
isLocal context node = (nodeID node) == (nodeID . localNode . localState) context


sendNode :: Context a -> RPCRequest -> NodeInfo a -> IO (RPCResult a)
sendNode context request node = (sendRPC context) (nodeAddr node) request


updateBucket :: State a -> [NodeInfo a] -> Int -> State a
updateBucket state bucket i =
  state { kBuckets = kBuckets state & element i .~ bucket }
