module Main where

import Data.Char
import System.Environment

import qualified Protocol
import qualified UDP

import Types
import UDP.Types


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
  api <- Protocol.bootstrap udp addr join

  putStrLn $ "Listening on " <> show bind
  if addr == bind then return () else do
    putStrLn $ "Advertising on " <> show addr

  return api


main :: IO ()
main = do
  api <- startAPI
  return ()


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
