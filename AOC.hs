{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

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

vtol :: Vector a -> [a]
vtol = V.toList

tr :: Ord a => [a] -> [a] -> [a] -> [a]
tr xs ys = map ((M.fromList $ zip xs ys) M.!)

readBin :: String -> Int
readBin = foldl' (\x y -> x * 2 + digitToInt y) 0

summarize :: Ord n => ([n], n -> [(v, n)]) -> w -> ([(v, w)] -> w) -> n -> w
summarize (nodes, getChildren) v0 f n = head $ mapMaybe (M.!? n) $ iterate getAll M.empty
  where
    containedByAll xs = filter (matchesAll xs) nodes
    matchesAll ns n = null . (\\ ns) $ map snd $ getChildren n

    getAll m = foldl' add m $ containedByAll $ M.keys m

    add m n = M.insertWith (\_ x -> x) n (calc m $ getChildren n) m
    calc _ [] = v0
    calc m xs = f $ map (fmap (m M.!)) xs

converge :: Eq a => (a -> a) -> a -> a
converge f x = let x' = f x in if x' == x then x else converge f x'

instance (Num a, Num b) => Num (a, b) where
  (x, y) + (u, v) = (x + u, y + v)
  (x, y) * (u, v) = (x * u, y * v)
  negate (x, y) = (negate x, negate y)
  fromInteger x = (fromInteger x, 0)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)

(*$) :: Int -> (Int, Int) -> (Int, Int)
n *$ (x, y) = (n * x, n * y)

mapnbs :: [(Int, Int)] -> (a -> [a] -> b) -> Vector (Vector a) -> Vector (Vector b)
mapnbs nbs f m = V.imap (\y v -> V.imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (get x) nbs
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

maplos :: [(Int, Int)] -> (a -> Bool) -> (a -> [a] -> b) -> Vector (Vector a) -> Vector (Vector b)
maplos nbs isEmpty f m = V.imap (\y v -> V.imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (getFirst x) nbs
    getFirst x0 x = do
      v <- get x0 x
      if isEmpty v then getFirst (x0 + x) x else return v
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

nbs4, nbs8 :: [(Int, Int)]
nbs4 = [(0, 1), (-1, 0), (1, 0), (0, -1)]
nbs8 = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

map8nbs = mapnbs nbs8
map4nbs = mapnbs nbs4

map8los = maplos nbs8
map4los = maplos nbs4

dir 'E' = ( 1,  0)
dir 'N' = ( 0,  1)
dir 'W' = (-1,  0)
dir 'S' = ( 0, -1)

rot (x, y) = (-y, x)

rotn 0 = id
rotn n = rot . rotn ((n - 1) `mod` 4)

manhatten (x, y) = abs x + abs y
