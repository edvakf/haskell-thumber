{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (responseLBS, Application, rawPathInfo)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup)
import Data.Maybe

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  port <- getPort
  putStr "start Server: http://localhost:"
  print port
  run port helloApp

helloApp :: Application
helloApp req respond = respond $ responseLBS status200 [] (getPath req)

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000

getPath req = BL.fromStrict $ rawPathInfo req
