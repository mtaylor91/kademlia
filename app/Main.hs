module Main where

import Data.Char
import System.Environment

import qualified Protocol
import qualified UDP

import UDP.Types


defaultLocalHost :: String
defaultLocalHost = "localhost"


defaultLocalPort :: String
defaultLocalPort = "5000"


defaultBootstrapHost :: String
defaultBootstrapHost = "5000"


defaultBootstrapPort :: String
defaultBootstrapPort = "5000"


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


envBootstrapHost :: String
envBootstrapHost = "BOOTSTRAP_HOST"


envBootstrapPort :: String
envBootstrapPort = "BOOTSTRAP_PORT"


envBootstrapSuppress :: String
envBootstrapSuppress = "BOOTSTRAP_SUPPRESS"


bindAddress :: IO UDPAddr
bindAddress = do
  host <- envOrDefault [envBindHost, envHost] defaultLocalHost
  port <- envOrDefault [envBindPort, envPort] defaultLocalPort
  return $ UDPAddr host port


advertiseAddress :: IO UDPAddr
advertiseAddress = do
  host <- envOrDefault [envAdvertiseHost, envHost] defaultLocalHost
  port <- envOrDefault [envAdvertisePort, envPort] defaultLocalPort
  return $ UDPAddr host port


bootstrapAddress :: IO UDPAddr
bootstrapAddress = do
  host <- envOrDefault [envBootstrapHost] defaultBootstrapHost
  port <- envOrDefault [envBootstrapPort] defaultBootstrapPort
  return $ UDPAddr host port


bootstrapSuppress :: IO Bool
bootstrapSuppress = do
  maybeValue <- lookupEnv envBootstrapSuppress
  case maybeValue of
    Just value ->
      if fmap toUpper value == "FALSE" then return False else return True
    Nothing ->
      return False


main :: IO ()
main = do
  bind <- bindAddress
  addr <- advertiseAddress
  join <- bootstrapAddress

  let udp = UDP.protocol bind
  api <- Protocol.bootstrap udp addr $ Just join

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
