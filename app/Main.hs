module Main where

import System.Environment

import qualified Core
import qualified UDP

import UDP.Types


defaultHost :: String
defaultHost = "localhost"


defaultPort :: String
defaultPort = "5000"


envHost :: String
envHost = "HOST"


envPort :: String
envPort = "PORT"


envBindHost :: String
envBindHost = "BIND_HOST"


envBindPort :: String
envBindPort = "BIND_PORT"


envAdvertiseHost :: String
envAdvertiseHost = "ADVERTISE_HOST"


envAdvertisePort :: String
envAdvertisePort = "ADVERTISE_PORT"


bindAddress :: IO UDPAddr
bindAddress = do
  host <- envOrDefault [envBindHost, envHost] defaultHost
  port <- envOrDefault [envBindPort, envPort] defaultPort
  return $ UDPAddr host port


advertiseAddress :: IO UDPAddr
advertiseAddress = do
  host <- envOrDefault [envAdvertiseHost, envHost] defaultHost
  port <- envOrDefault [envAdvertisePort, envPort] defaultPort
  return $ UDPAddr host port


main :: IO ()
main = do
  kid <- Core.randomKID
  bind <- bindAddress
  addr <- advertiseAddress

  UDP.start bind addr kid

  putStrLn $ "Listening on udp://" <> hostname bind <> ":" <> udpPort bind
  if addr == bind then return () else do
    putStrLn $ "Advertising udp://" <> hostname addr <> ":" <> udpPort addr


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
