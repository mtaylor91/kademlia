module UDP.Encoding where

import Basement.Block (cast,singleton)
import Basement.Compat.IsList
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.UTF8 as UTF8

import Types (KID(..),NodeID(..),NodeInfo(..),RPCRequest(..),RPCResponse(..))
import UDP.Core
import UDP.Types


encode :: UDPMessage -> LBS.ByteString
encode message =
  BSB.toLazyByteString $ mconcat
    [ encodeKID $ udpRequestKID message
    , encodeKID $ udpSourceKID message
    , encodeAddr $ udpSourceAddr message
    , encodeData $ udpMessageData message
    ]


encodeKID :: KID -> BSB.Builder
encodeKID (KID w256) =
  mconcat $ fmap BSB.word8 $ toList $ cast $ singleton w256


encodeAddr :: UDPAddr -> BSB.Builder
encodeAddr (UDPAddr host port) =
  mconcat
    [ BSB.word8 $ fromIntegral $ length host
    , BSB.word8 $ fromIntegral $ length port
    , BSB.byteString $ UTF8.fromString host
    , BSB.byteString $ UTF8.fromString port
    ]


encodeData :: UDPMessageData -> BSB.Builder
encodeData messageData =
  case messageData of
    UDPRequest request ->
      encodeRequest request
    UDPResponse response ->
      encodeResponse response


encodeRequest :: RPCRequest -> BSB.Builder
encodeRequest request =
  case request of
    Ping ->
      BSB.word8 requestTypePing
    FindNodes (NodeID kid) ->
      mconcat
        [ BSB.word8 requestTypeFindNodes
        , encodeKID kid
        ]
    FindValue kid ->
      mconcat
        [ BSB.word8 requestTypeFindValue
        , encodeKID kid
        ]
    Store kid value ->
      mconcat
        [ BSB.word8 requestTypeStore
        , encodeKID kid
        , BSB.word8 $ fromIntegral $ BS.length value
        , BSB.byteString value
        ]


encodeResponse :: RPCResponse UDPAddr -> BSB.Builder
encodeResponse response =
  case response of
    Pong ->
      BSB.word8 responseTypePong
    FoundNodes nodes ->
      mconcat $
        BSB.word8 responseTypeFoundNodes :
        BSB.word8 (fromIntegral $ length nodes) :
        fmap encodeNode nodes
    FoundValue value ->
      mconcat
        [ BSB.word8 responseTypeFoundValue
        , BSB.word8 $ fromIntegral $ BS.length value
        , BSB.byteString value
        ]
    Stored kid ->
      mconcat
        [ BSB.word8 responseTypeStored
        , encodeKID kid
        ]


encodeNode :: NodeInfo UDPAddr -> BSB.Builder
encodeNode (NodeInfo (NodeID kid) addr) = encodeKID kid <> encodeAddr addr
