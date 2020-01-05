{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Kademlia.KID where

import Prelude hiding (toInteger)

import Basement.Bits (countLeadingZeros,isBitSet)
import Basement.Block (Block(..),cast,createFromPtr,index,singleton)
import Basement.Compat.IsList (toList)
import Basement.Numerical.Number (toInteger)
import Basement.Types.OffsetSize (CountOf(..),Offset(..))
import Basement.Types.Word256 (Word256(..),bitwiseXor)
import Crypto.Hash (Digest,SHA256,hash)
import Data.Binary.Get (Get,getWord64be,runGet)
import Data.Bits hiding (countLeadingZeros,xor)
import Data.Hex (unhex)
import Data.Word (Word8)
import System.Random (getStdGen,setStdGen,random)
import Text.Printf (printf)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8BS


newtype KID = KID { word256 :: Word256 }
  deriving (Eq,Ord,Show,Bits,Num)


kidBits :: Int
kidBits = kidBytes * 8


kidBytes :: Int
kidBytes = 32


getBucketIndex :: KID -> KID -> Int
getBucketIndex nodekid kid =
  (fromInteger $ toInteger $ countLeadingZeros $ xor nodekid kid) - 1


emptyKID :: KID
emptyKID = KID 0


filledKID :: KID
filledKID = KID $ 2^kidBits-1


fromHex :: BS.ByteString -> Maybe KID
fromHex bs = do
  bytes <- unhex bs
  decodeKID bytes


toHex :: KID -> LBS.ByteString
toHex k =
  LBS.foldr f "" $ BSB.toLazyByteString $ encodeKID k where
    f i a = LBS.append (s i) a
    s i = C8BS.pack $ printf "%02x" i


encodeKID :: KID -> BSB.Builder
encodeKID (KID w256) =
  mconcat $ fmap BSB.word64BE $ toList $ cast $ singleton w256


decodeKID :: BS.ByteString -> Maybe KID
decodeKID bytes = if BS.length bytes < kidBytes then Nothing else
  Just $ runGet d $ LBS.fromStrict bytes where
  d :: Get KID = do
    w64_0 <- getWord64be
    w64_1 <- getWord64be
    w64_2 <- getWord64be
    w64_3 <- getWord64be
    return $ KID $ Word256 w64_0 w64_1 w64_2 w64_3


bucketKID :: KID -> Int -> KID
bucketKID (KID w) b =
  if isBitSet w o then KID $ clearBit w i
                  else KID $ setBit w i
  where i = kidBits - 1 - b
        o = Offset i


setKIDPrefix :: Int -> KID -> KID -> KID
setKIDPrefix prefixLen prefixFrom suffixFrom =
  prefix .|. suffix where
  prefixLen' = if prefixLen < kidBits then prefixLen else kidBits
  prefixMask = complement $ 2 ^ (kidBits - prefixLen') - 1
  suffixMask = complement prefixMask
  prefix = prefixMask .&. prefixFrom
  suffix = suffixMask .&. suffixFrom


showBinary :: KID -> String
showBinary (KID w256) =
  f "" [0..kidBits - 1]
    where
      f a [] = a
      f a (i:remaining) =
        let a' = a <> if testBit w256 (kidBits - 1 - i) then "1" else "0"
         in f a' remaining


xor :: KID -> KID -> Word256
xor (KID k) (KID k') = bitwiseXor k k'


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


randomKIDInBucket :: KID -> Int -> IO KID
randomKIDInBucket kid bucketIndex = do
  randomKIDWithPrefix (bucketKID kid bucketIndex) (bucketIndex + 1)


randomKIDWithPrefix :: KID -> Int -> IO KID
randomKIDWithPrefix prefixFrom prefixBits = do
  suffixFrom <- randomKID
  return $ setKIDPrefix prefixBits prefixFrom suffixFrom


sha256KID :: BS.ByteString -> IO KID
sha256KID input = do
  let d :: Digest SHA256 = hash input
  buffer <- BA.withByteArray d $ \p ->
    (createFromPtr p $ CountOf kidBytes) :: IO (Block Word256)
  return $ KID $ index buffer (Offset 0)
