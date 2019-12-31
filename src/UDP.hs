module UDP (UDPAddr(..),udp) where

import Prelude hiding (lookup)

import Control.Concurrent
import Data.Map (empty,insert,lookup)
import qualified Network.Socket as NS

import Types
import UDP.Core
import UDP.Receive
import UDP.Send
import UDP.Types


udp :: UDPAddr -> UDPAddr -> KID -> IO (Protocol UDPAddr)
udp bindAddress advertiseAddress kid = do
  s <- start bindAddress advertiseAddress kid
  return $ Protocol advertiseAddress (send s) (receive s)


start :: UDPAddr -> UDPAddr -> KID -> IO UDPSocket
start bindAddress advertiseAddress kid = do
  mvar <- newMVar empty

  addr <- getSocketAddr bindAddress

  socket <- NS.socket
    (NS.addrFamily addr)
    (NS.addrSocketType addr)
    (NS.addrProtocol addr)

  let addRequest requestKID respond =
        modifyMVar_ mvar $ \active ->
          return $ insert requestKID respond active

  let getRequest requestKID =
        withMVar mvar $ \active ->
          return $ lookup requestKID active

  NS.bind socket $ NS.addrAddress addr

  return $ UDPSocket
    advertiseAddress
    addRequest
    getRequest
    socket
    kid
