module Kademlia where

import qualified Data.ByteString as BS

import Kademlia.KID
import Kademlia.Types


lookup :: API a -> KID -> IO (Maybe BS.ByteString)
lookup api = lookupValue api


insert :: API a -> BS.ByteString -> IO KID
insert api value = do
  kid <- sha256KID value
  _:_ <- (insertValue api) kid value
  return kid
