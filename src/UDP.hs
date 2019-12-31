module UDP (UDPAddr(..),start) where

import Prelude hiding (lookup)

import Control.Concurrent
import Data.Map (empty,insert,lookup)
import qualified Network.Socket as NS

import qualified Protocol
import Types
import UDP.Core
import UDP.Receive
import UDP.Send
import UDP.Types


start :: UDPAddr -> UDPAddr -> KID -> IO ()
start bindAddress advertiseAddress kid = do
  s <- bindSocket bindAddress advertiseAddress kid
  let udp = Protocol advertiseAddress (send s) (receive s)
  Protocol.start udp kid


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
