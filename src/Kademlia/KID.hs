{-# LANGUAGE ScopedTypeVariables #-}
module Kademlia.KID where

import Basement.Block (Block,cast,createFromPtr,index)
import Basement.Types.OffsetSize (CountOf(..),Offset(..))
import Basement.Types.Word256 (Word256,bitwiseXor)
import Crypto.Hash (Digest,SHA256,hash)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Word (Word8)
import System.Random (getStdGen,setStdGen,random)

import Kademlia.Types


kidBits :: Int
kidBits = kidBytes * 8


kidBytes :: Int
kidBytes = 32


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


xor :: KID -> KID -> Word256
xor (KID k) (KID k') = bitwiseXor k k'
