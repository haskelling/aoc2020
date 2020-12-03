module AOC(module Prelude, module AOC, module Text.Parsec) where

import Prelude hiding(interact)
import qualified Prelude
import Text.Parsec hiding(count, parse)
import qualified Text.Parsec as Parsec

interact :: Show a => ([String] -> a) -> IO ()
interact f = Prelude.interact $ (++"\n") . show . f . lines

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)

rights :: [Either a b] -> [b]
rights (Right x:xs) = x:rights xs
rights (_:xs) = rights xs
rights [] = []
