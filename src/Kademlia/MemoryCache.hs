{-# LANGUAGE RankNTypes #-}
module Kademlia.MemoryCache where

import Prelude hiding (lookup)

import Control.Concurrent

import qualified Data.ByteString as BS
import qualified Data.HashMap as HashMap

import Kademlia.Cache (Cache(..),Query,Store)
import Kademlia.KID (KID)


data MemoryCache = MemoryCache Query Store


instance Cache MemoryCache where
  query (MemoryCache q _) = q
  store (MemoryCache _ s) = s


data MemoryCacheEntry = MemoryCacheEntry
  { cacheEntryID :: KID
  , cacheEntryBytes :: BS.ByteString
  } deriving (Eq,Ord,Show)


memoryCache :: IO MemoryCache
memoryCache = do

  cacheIndex <- newMVar HashMap.empty

  let q kid = do
        entryMap <- readMVar cacheIndex
        return $ do
          entry <- HashMap.lookup kid entryMap
          return $ return $ cacheEntryBytes entry

      s kid bytes = do
        let entry = MemoryCacheEntry kid bytes
        modifyMVar_ cacheIndex $ \entryMap ->
          return $ HashMap.insert kid entry entryMap

  return $ MemoryCache q s
