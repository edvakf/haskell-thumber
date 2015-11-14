{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.TCP (HStream)
import Network.Stream (Result)
import Network.HTTP (Response, simpleHTTP, getResponseBody, defaultGETRequest_, rspCode, rspReason)
import Network.HTTP.Types (status200, status400)
import Network.HTTP.Types.Status (Status, mkStatus)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Headers (findHeader, HeaderName(HdrContentLength, HdrContentType))
import Network.URI (URI, parseURI)
import Network.Wai (Request, responseLBS, responseStream, StreamingBody, Application, pathInfo)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup, intercalate)
import Data.Maybe
import Data.Either

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Blaze.ByteString.Builder.ByteString as BB

main :: IO ()
main = do
  port <- getPort
  putStr "start Server: http://localhost:"
  print port
  run port thumberApp

thumberApp :: Application
thumberApp req respond =
  let url = urlFromPath req in
  case parseURI url of
    Nothing -> respond $ responseLBS status400 [] "Invalid URL"
    Just uri -> do
      result <- httpGet uri
      case result of
        Left _ ->
          respond $ responseLBS status400 [] "Upstream request failed"
        Right rsp ->
          respond $ responseStream (convStatus rsp) (convHeaders rsp) $ \write flush -> do
            body <- getResponseBody result
            write $ BB.fromByteString body

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000

urlFromPath :: Request -> String
urlFromPath req = (++) "http://" $ intercalate "/" $ map T.unpack $ pathInfo req

-- use defaultGETRequest_ instead of getRequest because of https://github.com/haskell/HTTP/issues/1
httpGet :: HStream ty => URI -> IO (Result (Response ty))
httpGet uri = simpleHTTP (defaultGETRequest_ uri)

-- convert upstream response to Wai's response status
convStatus :: HStream ty => Response ty -> Status
convStatus rsp =
  let (x,y,z) = rspCode rsp; reason = rspReason rsp in
  mkStatus (100*x+10*y+z) (C.pack reason)

-- convert upstream response to Wai's response headers
convHeaders :: HStream ty => Response ty -> ResponseHeaders
convHeaders rsp =
  let contentType = fromMaybe "application/octet-stream" (findHeader HdrContentType rsp) in
  let contentLength = fromMaybe "0" (findHeader HdrContentLength rsp) in
  [("Content-Type", C.pack contentType), ("Content-Length", C.pack contentLength)]
