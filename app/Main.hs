{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (responseLBS, Application, pathInfo)
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run, Port)
import System.Environment (getEnvironment)
import Data.List (lookup, intercalate)
import Data.Maybe

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  port <- getPort
  putStr "start Server: http://localhost:"
  print port
  run port helloApp

helloApp :: Application
helloApp req respond = respond $ responseLBS status200 [] (C.pack (urlFromPath req))

getPort :: IO Port
getPort = getEnvironment >>= return . port
  where
    port = fromMaybe defaultPort . fmap read . lookup "PORT"

defaultPort :: Port
defaultPort = 3000

urlFromPath req = ("http://" :: String) ++ (intercalate "/" (map T.unpack (pathInfo req)))
