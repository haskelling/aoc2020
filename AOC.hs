{-# LANGUAGE ScopedTypeVariables #-}

module AOC(module Prelude, module AOC, module Text.Parsec, module Data.Vector) where

import Data.Char
import Prelude hiding(interact)
import qualified Prelude
import Text.Parsec hiding(count, parse)
import qualified Text.Parsec as Parsec
import Data.Vector(Vector)
import qualified Data.Vector as V

interact :: Show a => ([String] -> a) -> IO ()
interact f = interact' $ f . lines

interact' :: Show a => (String -> a) -> IO ()
interact' f = Prelude.interact $ (++"\n") . show . f

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

chari :: Char -> Parser Char
chari c = oneOf [toLower c, toUpper c]

stringi :: String -> Parser String
stringi = mapM chari

enump :: forall b. (Enum b, Bounded b, Show b) => Parser b
enump = choice $ map sr [minBound :: b .. maxBound :: b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)

rights :: [Either a b] -> [b]
rights (Right x:xs) = x:rights xs
rights (_:xs) = rights xs
rights [] = []

(!|) :: Vector a -> Int -> a
v !| i = v V.! (i `rem` V.length v)

ltov :: [a] -> Vector a
ltov = V.fromList
