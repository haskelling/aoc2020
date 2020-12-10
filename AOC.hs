{-# LANGUAGE ScopedTypeVariables #-}

module AOC(module Prelude, module AOC, module Text.Parsec, module Data.Vector, module Data.Char, module Data.List, module Data.List.Split, module Data.Hashable, module Data.Maybe, module Data.Either) where

import Data.Char
import Data.Maybe
import Data.Either
import Prelude hiding(interact)
import qualified Prelude
import Text.Parsec hiding(count, parse, uncons)
import qualified Text.Parsec as Parsec
import Data.Vector(Vector)
import qualified Data.Vector as V
import Data.List
import qualified Data.Map as M
import Data.List.Split hiding(oneOf, sepBy, endBy)
import Data.Hashable(hash)

interact :: Show a => ([String] -> a) -> IO ()
interact f = interact' $ f . lines

interact' :: Show a => (String -> a) -> IO ()
interact' f = Prelude.interact $ (++"\n") . show . f

interactg :: Show a => ([[String]] -> a) -> IO ()
interactg f = interact $ f. splitOn [""]

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parselist :: Parser a -> [String] -> [a]
parselist p = either (error . show) id . mapM (parse p)

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

(!|) :: Vector a -> Int -> a
v !| i = v V.! (i `rem` V.length v)

ltov :: [a] -> Vector a
ltov = V.fromList

tr :: Ord a => [a] -> [a] -> [a] -> [a]
tr xs ys = map ((M.fromList $ zip xs ys) M.!)

readBin :: String -> Int
readBin = foldl' (\x y -> x * 2 + digitToInt y) 0

summarize :: Ord n => ([n], n -> [(v, n)]) -> w -> ([(v, w)] -> w) -> n -> w
summarize (nodes, getChildren) v0 f n = head $ catMaybes $ map (M.!? n) $ iterate getAll M.empty
  where
    containedByAll xs = filter (matchesAll xs) nodes
    matchesAll ns n = null . (\\ ns) $ map snd $ getChildren n

    getAll m = foldl' add m $ containedByAll $ M.keys m

    add m n = M.insertWith (flip const) n (calc m $ getChildren n) m
    calc _ [] = v0
    calc m xs = f $ map (fmap (m M.!)) xs
