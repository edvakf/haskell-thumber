{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.TCP (HStream)
import Network.Stream (Result)
import Network.HTTP (Response, simpleHTTP, getResponseBody, defaultGETRequest_, rspCode, rspReason)
import Network.HTTP.Types (status200, status400)
import Network.HTTP.Types.Status (Status, mkStatus)
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Headers (findHeader, HeaderName(HdrContentLength, HdrContentType))
import Network.URI (URI, parseURI)
import Network.Wai (Request, responseLBS, responseStream, StreamingBody, Application, pathInfo)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup, intercalate)
import Data.Maybe
import Data.Either
import Lib.Config (getConfig, Config, getWidth, getHeight)
import Lib.Image (convert)

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Blaze.ByteString.Builder.ByteString as BB
import qualified Data.ByteString.Lazy.Char8 as LC

main :: IO ()
main = do
  port <- getPort
  putStr "start Server: http://localhost:"
  print port
  run port thumberApp

thumberApp :: Application
thumberApp req respond =
  case parsePath req of
    Left err -> respond $ responseLBS status400 [] $ LC.pack err
    Right (config, uri) -> do
      (print $ "Requesting: " ++ (show uri))
      (print $ "Config: " ++ (show config))
      result <- httpGet uri
      case result of
        Left _ ->
          respond $ responseLBS status400 [] "Upstream request failed"
        Right rsp ->
          case rspCode rsp of
            (2, _, _) -> do
              body <- getResponseBody result
              case convert (getWidth config, getHeight config) body of
                Left err -> respond $ responseLBS status400 [] $ LC.pack err
                Right converted -> respond $ responseLBS status200 [contentTypeHeader rsp, contentLengthHeader converted] converted
            otherwise ->
              respond $ responseLBS status400 [] "Upstream response is not 2XX"

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000

parsePath :: Request -> Either String (Config, URI)
parsePath req =
  if null pathList then Left "Invalid path"
  else case getConfig (head pathList) of
    Left err -> Left err
    Right c -> case parseURI upstreamURL of
      Nothing -> Left "Invalid path"
      Just u -> Right (c, u)
  where
    pathList = map T.unpack $ pathInfo req
    upstreamURL = (++) "http://" $ intercalate "/" $ tail pathList

-- use defaultGETRequest_ instead of getRequest because of https://github.com/haskell/HTTP/issues/1
httpGet :: HStream ty => URI -> IO (Result (Response ty))
httpGet uri = simpleHTTP (defaultGETRequest_ uri)

contentTypeHeader :: HStream ty => Response ty -> Header
contentTypeHeader rsp =
  ("Content-Type", C.pack contentType)
  where
    contentType = fromMaybe "application/octet-stream" (findHeader HdrContentType rsp)

contentLengthHeader :: LB.ByteString -> Header
contentLengthHeader content =
  ("Content-Length", C.pack $ show $ LB.length content)
