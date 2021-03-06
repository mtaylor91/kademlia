module Kademlia.UDP (UDPAddr(..),protocol) where

import Prelude hiding (lookup)

import Control.Concurrent
import Data.Map (empty,insert,lookup,delete)
import qualified Network.Socket as NS

import Kademlia.KID
import Kademlia.RPC
import Kademlia.UDP.Core
import Kademlia.UDP.Receive
import Kademlia.UDP.Send
import Kademlia.UDP.Types


protocol :: UDPAddr -> UDPAddr -> KID -> IO (Protocol UDPAddr)
protocol bindAddress advertiseAddress kid = do
  s <- bindSocket bindAddress advertiseAddress kid
  return $ Protocol advertiseAddress (send s) (receive s)


bindSocket :: UDPAddr -> UDPAddr -> KID -> IO UDPSocket
bindSocket bindAddress advertiseAddress kid = do
  mvar <- newMVar empty

  addr <- getSocketAddr bindAddress

  socket <- NS.socket
    (NS.addrFamily addr)
    (NS.addrSocketType addr)
    (NS.addrProtocol addr)

  let addRequest requestKID respond =
        modifyMVar_ mvar $ \active ->
          return $ insert requestKID respond active

      getRequest requestKID =
        withMVar mvar $ \active ->
          return $ lookup requestKID active

      cancel requestKID =
        modifyMVar_ mvar $ \active ->
          return $ delete requestKID active

  NS.bind socket $ NS.addrAddress addr

  return $ UDPSocket
    advertiseAddress
    addRequest
    getRequest
    cancel
    socket
    kid
