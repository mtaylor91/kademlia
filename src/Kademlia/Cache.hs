{-# LANGUAGE RankNTypes #-}
module Kademlia.Cache where

import qualified Data.ByteString as BS

import Kademlia.KID


type Query = KID -> IO (Maybe (IO BS.ByteString))


type Store = KID -> BS.ByteString -> IO ()


class Cache c where
  query :: c -> Query
  store :: c -> Store
