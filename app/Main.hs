module Main where

import Data.Char
import System.Environment

import qualified Kademlia.Controller as Controller
import qualified Kademlia.UDP as UDP

import Kademlia.API
import Kademlia.KID
import Kademlia.HTTP
import Kademlia.UDP.Types


defaultLocalHost :: String
defaultLocalHost = "localhost"


defaultLocalPort :: String
defaultLocalPort = "5000"


defaultBootstrapHost :: String
defaultBootstrapHost = "localhost"


defaultBootstrapPort :: String
defaultBootstrapPort = "5000"


envHost :: String
envHost = "HOST"


envPort :: String
envPort = "PORT"


envAPIHost :: String
envAPIHost = "API_HOST"


envAPIPort :: String
envAPIPort = "API_PORT"


envDHTHost :: String
envDHTHost = "DHT_HOST"


envDHTPort :: String
envDHTPort = "DHT_PORT"


envAdvertiseHost :: String
envAdvertiseHost = "ADVERTISE_HOST"


envAdvertisePort :: String
envAdvertisePort = "ADVERTISE_PORT"


envBootstrapHost :: String
envBootstrapHost = "BOOTSTRAP_HOST"


envBootstrapPort :: String
envBootstrapPort = "BOOTSTRAP_PORT"


envBootstrapSuppress :: String
envBootstrapSuppress = "BOOTSTRAP_SUPPRESS"


bindAddress :: IO UDPAddr
bindAddress = do
  host <- envOrDefault [envDHTHost, envHost] defaultLocalHost
  port <- envOrDefault [envDHTPort, envPort] defaultLocalPort
  return $ UDPAddr host port


advertiseAddress :: IO UDPAddr
advertiseAddress = do
  host <- envOrDefault [envAdvertiseHost, envHost] defaultLocalHost
  port <- envOrDefault [envAdvertisePort, envPort] defaultLocalPort
  return $ UDPAddr host port


bootstrapAddress :: IO (Maybe UDPAddr)
bootstrapAddress = do
  maybeValue <- lookupEnv envBootstrapSuppress
  case maybeValue of
    Just value ->
      if fmap toUpper value /= "FALSE" then return Nothing else addr
    Nothing ->
      addr
  where
    addr = do
      host <- envOrDefault [envBootstrapHost] defaultBootstrapHost
      port <- envOrDefault [envBootstrapPort] defaultBootstrapPort
      return $ Just $ UDPAddr host port


startAPI :: IO (API UDPAddr)
startAPI = do
  bind <- bindAddress
  addr <- advertiseAddress
  join <- bootstrapAddress

  let udp = UDP.protocol bind
  kid <- randomKID
  api <- Controller.start kid udp addr join

  putStrLn $ "Listening on " <> show bind
  if addr == bind then return () else do
    putStrLn $ "Advertising on " <> show addr

  return api


main :: IO ()
main = do
  apiPortS <- envOrDefault [envAPIPort, envPort] defaultLocalPort
  let apiPort = read apiPortS
  api <- startAPI
  putStrLn $ "Starting API server at http://localhost:" <> apiPortS
  server api apiPort


envOrDefault :: [String] -> String -> IO String
envOrDefault envKeys defaultValue =
  case envKeys of
    envKey:envKeys' -> do
      maybeValue <- lookupEnv envKey
      case maybeValue of
        Just value -> return value
        Nothing -> envOrDefault envKeys' defaultValue
    [] ->
      return defaultValue
