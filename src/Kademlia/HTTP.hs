{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
module Kademlia.HTTP where

import Conduit
import Yesod
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8BS
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as T

import Kademlia (lookup,insert)
import Kademlia.API (API)
import Kademlia.KID (KID,fromHex,toHex)


data RESTAPI = forall a. RESTAPI (API a)


newtype PathKID = PathKID KID deriving (Eq,Show)


instance PathPiece PathKID where
  toPathPiece (PathKID k) = T.decodeUtf8 $ LBS.toStrict $ toHex k
  fromPathPiece t = do
    k <- fromHex $ T.encodeUtf8 t
    return $ PathKID k


instance Read PathKID where
  readsPrec _ s = if length s < 64 then [] else
    case fromHex $ LBS.toStrict $ C8BS.pack $ take 64 s of
      Just k -> [(PathKID k, drop 64 s)]
      Nothing -> []


mkYesod "RESTAPI" [parseRoutes|
/               RootR   GET POST
/#PathKID       PathR   GET
|]


instance Yesod RESTAPI


getRootR :: HandlerFor RESTAPI Value
getRootR = do
  returnJson ("status placeholder" :: String)


postRootR :: HandlerFor RESTAPI TypedContent
postRootR = do
  RESTAPI api <- getYesod
  chunks <- runConduit $ rawRequestBody .| CL.consume
  kid <- liftIO $ Kademlia.insert api $ mconcat chunks
  respond "text/plain" $ LBS.append (toHex kid) "\n"


getPathR :: PathKID -> HandlerFor RESTAPI TypedContent
getPathR (PathKID kid) = do
  RESTAPI api <- getYesod
  maybeValue <- liftIO $ Kademlia.lookup api kid
  case maybeValue of
    Just value ->
      respond "application/octet-stream" value
    Nothing ->
      notFound


server :: forall a. API a -> Int -> IO ()
server api port = do
  warp port $ RESTAPI api
