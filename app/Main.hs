{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (responseLBS, responseStream, StreamingBody, Application, pathInfo)
import Network.HTTP.Types (status200, status400)
import Network.HTTP (simpleHTTP, getResponseBody, defaultGETRequest_)
import Network.URI (URI, parseURI)
import Network.Wai (Request, Response)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup, intercalate)
import Data.Maybe

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Blaze.ByteString.Builder.ByteString as BB

main :: IO ()
main = do
  port <- getPort
  putStr "start Server: http://localhost:"
  print port
  run port helloApp

helloApp :: Application
helloApp req respond = respond $ handleRequest req

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000

urlFromPath :: Request -> String
urlFromPath req = (++) "http://" $ intercalate "/" $ map T.unpack $ pathInfo req

handleRequest :: Request -> Response
handleRequest req =
  let url = urlFromPath req in
  case parseURI url of
    Nothing -> responseLBS status400 [] "Invalid URL"
    Just uri -> responseStream status200 [("Content-Type", "image/jpeg")] (lazyByteStringToStream (httpGetLazyByteString uri))

httpGetLazyByteString :: URI -> IO B.ByteString
httpGetLazyByteString uri = do
  -- use defaultGETRequest_ instead of getRequest because of https://github.com/haskell/HTTP/issues/1
  rsp <- simpleHTTP (defaultGETRequest_ uri)
  body <- getResponseBody rsp
  return body

lazyByteStringToStream :: IO B.ByteString -> StreamingBody
lazyByteStringToStream iobs = \write flush -> do
  bs <- iobs
  write $ BB.fromByteString bs
  flush
