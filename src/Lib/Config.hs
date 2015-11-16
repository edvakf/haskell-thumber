module Lib.Config
    ( getConfig
    , Config
    , getWidth
    , getHeight
    ) where

import Data.Char (digitToInt)
import Data.Maybe (isJust)
import Data.Either
import Control.Applicative ((*>))
import Text.Parsec (many, (<|>), sepBy1, parse)
import Text.Parsec.Char (char, digit)
import Text.Parsec.String (Parser)
import Data.List (foldl')

data KeyVal = Width Int
            | Height Int
            deriving (Show)

type Config = [KeyVal]

atoi :: [Char] -> Int
atoi a = foldl' (\num x -> num * 10 + (digitToInt x)) 0 a

parseConfig :: Parser Config
parseConfig = parseKeyVal `sepBy1` (char ',')

parseKeyVal :: Parser KeyVal
parseKeyVal = parseWidth <|> parseHeight

parseWidth :: Parser KeyVal
parseWidth = do
  char 'w'
  char '='
  ds <- many digit
  return $ Width (atoi ds)

parseHeight :: Parser KeyVal
parseHeight = do
  char 'h'
  char '='
  ds <- many digit
  return $ Height (atoi ds)

getConfig :: String -> Either String Config
getConfig input = case parse parseConfig "Config Error!" input of
  Left err -> Left $ show err
  Right c
    | isJust (getWidth c) -> Right c
    | isJust (getHeight c) -> Right c
    | otherwise -> Left "Either width or height must be supplied"

getWidth :: Config -> Maybe Int
getWidth [] = Nothing
getWidth ((Width w):kvs) = Just w
getWidth (kv:kvs) = getWidth kvs

getHeight :: Config -> Maybe Int
getHeight [] = Nothing
getHeight ((Height h):kvs) = Just h
getHeight (kv:kvs) = getHeight kvs

