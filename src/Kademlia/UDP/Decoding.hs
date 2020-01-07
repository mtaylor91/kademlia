module Kademlia.UDP.Decoding where

import Control.Monad
import Data.Binary.Get (runGet,getWord64be)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Word (Word8,Word64)

import Kademlia.KID (KID)
import Kademlia.NodeInfo (NodeInfo(..))
import Kademlia.RPC (RPCRequest(..),RPCResponse(..))
import Kademlia.UDP.Core
import Kademlia.UDP.Types
import qualified Kademlia.KID as KID


newtype Decoder a = Decoder (BS.ByteString -> Maybe (a, BS.ByteString))


instance Functor Decoder where
  fmap = liftM


instance Applicative Decoder where
  pure a = Decoder $ \b -> Just (a, b)
  (<*>) = ap


instance Monad Decoder where
  Decoder decoder >>= next =
    Decoder $ \bytes -> do
      (value, bytes') <- decoder bytes
      let Decoder decoder' = next value
      decoder' bytes'


decode :: BS.ByteString -> Maybe UDPMessage
decode bytes =
  case output bytes decodeUDPMessage of
    Just (message, _) ->        Just message
    Nothing ->                  Nothing


invalid :: Decoder void
invalid = Decoder $ \_ -> Nothing


output :: BS.ByteString -> Decoder a -> Maybe (a, BS.ByteString)
output bytes (Decoder d) = d bytes


decodeUDPMessage :: Decoder UDPMessage
decodeUDPMessage = do
  requestKID <- decodeKID
  sourceKID <- decodeKID
  sourceAddr <- decodeAddr
  messageData <- decodeData
  return $ UDPMessage
    { udpRequestKID = requestKID
    , udpSourceKID = sourceKID
    , udpSourceAddr = sourceAddr
    , udpMessageData = messageData
    }


decodeData :: Decoder UDPMessageData
decodeData = do
  messageType <- decodeWord8
  case messageType of

    m | m == requestTypePing ->
      return $ UDPRequest Ping

    m | m == requestTypeFindNodes -> do
      kid <- decodeKID
      return $ UDPRequest $ FindNodes kid

    m | m == requestTypeFindValue -> do
      kid <- decodeKID
      return $ UDPRequest $ FindValue kid

    m | m == requestTypeStore -> do
      kid <- decodeKID
      len <- decodeWord64BE
      value <- decodeBytes $ fromIntegral len
      return $ UDPRequest $ Store kid value

    m | m == responseTypePong ->
      return $ UDPResponse Pong

    m | m == responseTypeFoundNodes -> do
      count <- decodeWord8
      nodes <- sequence $ take (fromIntegral count) $ repeat decodeNode
      return $ UDPResponse $ FoundNodes nodes

    m | m == responseTypeFoundValue -> do
      len <- decodeWord64BE
      value <- decodeBytes $ fromIntegral len
      return $ UDPResponse $ FoundValue value

    m | m == responseTypeStored -> do
      kid <- decodeKID
      return $ UDPResponse $ Stored kid

    _ ->
      invalid


decodeNode :: Decoder (NodeInfo UDPAddr)
decodeNode = do
  kid <- decodeKID
  addr <- decodeAddr
  return $ NodeInfo kid addr


decodeKID :: Decoder KID
decodeKID = Decoder $ \bytes -> do
  kid <- KID.decodeKID bytes
  return (kid, BS.drop KID.kidBytes bytes)


decodeAddr :: Decoder UDPAddr
decodeAddr = do
  hostLen <- decodeWord8
  portLen <- decodeWord8
  host <- decodeString $ fromIntegral hostLen
  port <- decodeString $ fromIntegral portLen
  return $ UDPAddr host port


decodeBytes :: Int -> Decoder BS.ByteString
decodeBytes len = Decoder $ \bytes ->
  case BS.length bytes of
    l | l >= len ->
      Just (BS.take len bytes, BS.drop len bytes)
    _ ->
      Nothing


decodeString :: Int -> Decoder String
decodeString len = do
  bytes <- decodeBytes len
  return $ UTF8.toString bytes


decodeWord8 :: Decoder Word8
decodeWord8 = Decoder $ \bytes ->
  case BS.length bytes of
    l | l >= 1 ->       Just (BS.index bytes 0, BS.drop 1 bytes)
    _ ->                Nothing


decodeWord64BE :: Decoder Word64
decodeWord64BE = Decoder $ \bytes ->
  case BS.length bytes of
    l | l >= 8 ->
      let w64 = runGet getWord64be $ LBS.fromStrict bytes
       in return (w64, BS.drop 8 bytes)
    _ ->
      Nothing
