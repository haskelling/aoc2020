{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}
{-|
Module      : AOC
Description : A library of useful functions for solving coding problems
Copyright   : (c) 2020 Haskell Coder
License     : GPL-3
Maintainer  : haskelling@pdr.cx
Stability   : experimental
Portability : POSIX

AOC is a library of useful functions for solving coding problems.

It accompanies a [video series](https://www.youtube.com/playlist?list=PLDRIsR-OaZkzN7iV6Q6MRmEVYL_HRz7GS)
describing how to solve the [2020 Advent of Code challenges](https://www.adventofcode.com/2020).
It is intended as a learning tool for beginner and intermediate Haskellers, so
often goes into some depth describing Haskell concepts, tools, and library
functions.

The code for this module along with the 2020 AoC solutions can be cloned from [GitHub](https://github.com/haskelling/aoc2020).
-}
module AOC(module Prelude, module AOC, module Text.Parsec, module Data.Vector, module Data.Char, module Data.List, module Data.List.Split, module Data.Hashable, module Data.Maybe, module Data.Either) where

import Data.Char
import Data.Maybe
import Data.Either
import Prelude hiding(interact)
import qualified Prelude
import Text.Parsec hiding(count, parse, uncons)
import qualified Text.Parsec as Parsec
import Data.Vector(Vector, imap)
import qualified Data.Vector as V
import Data.List
import qualified Data.Map as M
import Data.List.Split hiding(oneOf, sepBy, endBy)
import Data.Hashable(hash)
import Control.Exception(ArithException(..))

-- * Enhanced interact functions

-- | The 'interact' function replaces 'Prelude.interact'.  It reads from stdin
-- and splits the input into 'lines'.  It uses the given function to turn the
-- resulting list of 'String's into a 'Show'able value.  It 'show's the value
-- and sends it to stdout with an added newline.
--
-- >>> sumValuesProg = interact (sum . map read)
--
interact :: Show a => ([String] -> a) -> IO ()
interact f = interact' $ f . lines

-- | The 'interact'' function reads from stdin and uses the given function to
-- run the resulting 'String' into a 'Show'able value.  It 'show's the value and
-- sends it to stdout with an added newline.
--
-- >>> fileSizeProg = interact' length
--
interact' :: Show a => (String -> a) -> IO ()
interact' f = Prelude.interact $ (++"\n") . show . f

-- | The 'interactg' function will not only split by 'lines', like 'interact',
-- but also splits these into groups separated by blank lines.  Therefore, the
-- given function must accept a list of list of 'String's.
--
-- >>> countParagraphsProg = interactg length
--
interactg :: Show a => ([[String]] -> a) -> IO ()
interactg f = interact $ f. splitOn [""]

-- * Parsing and Reading

-- ** Parsing

-- | 'Parser' is a convenience type for 'Parsec'
type Parser = Parsec String ()

-- | The 'parse' function is a convenience function for 'Parsec.parse' that
-- removes the requirement to provide a file name.
parse :: Parser a            -- ^ The parser for "a"s
      -> String              -- ^ The string to be parsed
      -> Either ParseError a -- ^ The successfully parsed value or an error
parse p = Parsec.parse p ""

-- | The 'parselist' function parses a list of 'String's using 'parse' and
-- returns the list of parsed "a"s.  If any parse was unsuccessful we crash the
-- program, showing the first error encountered.
parselist :: Parser a -- ^ The parser for "a"s
          -> [String] -- ^ The list of 'String's to parse
          -> [a]      -- ^ The resulting list of "a"s
parselist p = either (error . show) id . mapM (parse p)

-- | The 'chari' function is a case-insensitive 'Parser' for the given 'Char'.
-- It uses 'toLower' and 'toUpper' on the given 'Char' to test the input with.
--
-- >>> parse (chari 'e') "E"
-- Right 'E'
--
chari :: Char -> Parser Char
chari c = oneOf [toLower c, toUpper c]

-- | The 'stringi' function is a case-insensitive 'Parser' for the given
-- 'String'.
--
-- >>> parse (stringi "HeLlO") "Hello world!"
-- Right "Hello"
--
stringi :: String -> Parser String
stringi = mapM chari

-- | The 'enump' function is a case-insensitive 'Parser' for any 'Bounded'
-- 'Enum'.
--
-- >>> data Primary = Red | Green | Blue deriving (Show, Enum, Bounded)
-- >>> parselist enump $ words "red green red blue" :: [Primary]
-- [Red,Green,Red,Blue]
--
enump :: forall b. (Enum b, Bounded b, Show b) => Parser b
enump = choice $ map sr [minBound :: b .. maxBound :: b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x

-- | The 'integer function is a 'Parser' for unsigned 'Int's.
--
-- >>> parse integer "301"
-- Right 301
--
integer :: Parser Int
integer = read <$> many1 digit

-- ** Reading

-- |The 'readBin' function reads a binary number from a String.
-- Input is read until the first non-binary digit, after which the rest of the
-- String is silently dropped.
--
-- >>> readBin "01011010"
-- 90
--
readBin :: String -> Int
readBin = foldl' (\x y -> x * 2 + digitToInt y) 0 . takeWhile (`elem` "01")

-- * List functions

-- | The 'count' function returns the number of occurrences of the given value
-- in the given list.
--
-- >>> count 'e' "Advent of Code 2020"
-- 2
--
-- >>> count 2 [3,2,1,0,4,2,3,4,2]
-- 3
--
count :: Eq a
      => a   -- ^ The value to look for
      -> [a] -- ^ The list to look in
      -> Int -- ^ The number of times the value is found in the list
count c = length . filter (==c)

-- |The 'tr' function translates lists according to a given mapping.
--
-- >>> tr "LR" "01" "LALR"
-- "0A01"
--
tr :: Ord a
   => [a] -- ^ The "from" part of the mapping
   -> [a] -- ^ The "to" part of the mapping
   -> [a] -- ^ The original list
   -> [a] -- ^ The updated list after replacing "from" elements with their "to" counterparts
tr xs ys = map (\x -> fromMaybe x $ M.fromList (zip xs ys) M.!? x)

-- * Vector functions

-- | The '!|' operator indexes into a 'Vector' modulo its length.  This will
-- crash with a 'DivideByZero' 'ArithException' if the 'Vector' is empty. O(1)
--
-- >>> ltov [0..99] !| 254375
-- 75
--
(!|) :: Vector a -> Int -> a
v !| i = v V.! (i `mod` V.length v)

-- | The 'ltov' function is a convenience function for 'Vector.fromList'.
ltov :: [a] -> Vector a
ltov = V.fromList

-- | The 'ltov2' function converts a list of lists into a 'Vector' of 'Vector's.
ltov2 :: [[a]] -> Vector (Vector a)
ltov2 = ltov . map ltov

-- | The 'vtol' function is a convenience function for 'Vector.toList'.
vtol :: Vector a -> [a]
vtol = V.toList

-- | The 'vtol2' function converts a 'Vector' of 'Vector's into a list of lists.
vtol2 :: Vector (Vector a) -> [[a]]
vtol2 = map vtol . vtol

-- * Algorithms

-- ** Directed Acyclic Graph Algorithms

-- |The 'summarize' function does a bottom-up calculation on a directed
-- acyclic graph (DAG) by running a summary function on each node.
--
-- >>> summarize ([1..10], \x -> [(0, y) | y <- [2*x, 3*x..10]]) 1 (sum . map snd) 1
-- 13
--
-- >>> summarize ([1..10], \x -> [(y, y) | y <- [2*x, 3*x..10]]) 1 (sum . map (\(w, s) -> w * s)) 1
-- 279
--
summarize :: Ord n
          => ([n], n -> [(v, n)]) -- ^ The list of nodes and function from node to list of children, given as a tuple of edge "weight" and child node
          -> w                    -- ^ Summary value for childless nodes
          -> ([(v, w)] -> w)      -- ^ Summary function to populate the summary value for each node given a list of tuples representing the edge "weight" and summary value for each child node
          -> n                    -- ^ The node for which we need the summary
          -> w                    -- ^ The resulting summary value
summarize (nodes, getChildren) v0 f n = head $ mapMaybe (M.!? n) $ iterate getAll M.empty
  where
    containedByAll xs = filter (matchesAll xs) nodes
    matchesAll ns n = null . (\\ ns) $ map snd $ getChildren n

    getAll m = foldl' add m $ containedByAll $ M.keys m

    add m n = M.insertWith (\_ x -> x) n (calc m $ getChildren n) m
    calc _ [] = v0
    calc m xs = f $ map (fmap (m M.!)) xs

-- ** General-purpose Algorithms

-- |The 'converge' function repeatedly applies f until there's no change
-- in the output.  That is, it calculates \( f (f (f ... (f x))) \).
converge :: Eq a => (a -> a) -> a -> a
converge f x = let x' = f x in if x' == x then x else converge f x'

-- ** Grid Algorithms

-- |The 'mapnbs' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- Given a list of coordinate offsets, the mapping function is provided the
-- cell value, and a list of the values of the cells at those offsets.
mapnbs :: [(Int, Int)]      -- ^ The list of coordinate offsets
       -> (a -> [a] -> b)   -- ^ The mapping function
       -> Vector (Vector a) -- ^ The original grid
       -> Vector (Vector b) -- ^ The updated grid
mapnbs nbs f m = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (get x) nbs
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

-- |The 'maplos' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- A coordinate list is supplied and the given predicate determines what is
-- considered an empty cell.  The mapping function is provided the cell value,
-- and a list of cell values that are in the line of sight, in the direction of
-- the given coordinates.
maplos :: [(Int, Int)]      -- ^ The list of coordinates indicating the line-of-sight directions
       -> (a -> Bool)       -- ^ The isEmpty predicate - line of sight continues through cells whose value returns 'True'
       -> (a -> [a] -> b)   -- ^ The mapping function, given the cell value and the values of all cells found through a line-of-sight search
       -> Vector (Vector a) -- ^ The original grid
       -> Vector (Vector b) -- ^ The updated grid
maplos nbs isEmpty f m = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (getFirst x) nbs
    getFirst x0 x = do
      v <- get x0 x
      if isEmpty v then getFirst (x0 + x) x else return v
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

nbs4, nbs8 :: [(Int, Int)]
-- |'nbs4' lists the offsets of the four non-diagonal neighbours of a cell
-- in a grid.
nbs4 = [(0, 1), (-1, 0), (1, 0), (0, -1)]
-- |'nbs8' lists the offsets of the eight neighbours of a cell in a grid.
nbs8 = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

-- |The 'map8nbs' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- The mapping function is provided the cell value, and a list of the cell
-- values of its eight neighbours.
--
-- >>> conwayRule x ns = let n = count True ns in n == 3 || x && n == 2
-- >>> map8nbs conwayRule $ ltov2 $ replicate 5 $ replicate 5 True
-- [[True,False,False,False,True],[False,False,False,False,False],[False,False,False,False,False],[False,False,False,False,False],[True,False,False,False,True]]
--
map8nbs = mapnbs nbs8
-- |The 'map4nbs' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- The mapping function is provided the cell value, and a list of the cell
-- values of its four non-diagonal neighbours.
--
-- >>> conwayRule x ns = let n = count True ns in n == 3 || x && n == 2
-- >>> map4nbs conwayRule $ ltov2 $ replicate 5 $ replicate 5 True
-- [[True,True,True,True,True],[True,False,False,False,True],[True,False,False,False,True],[True,False,False,False,True],[True,True,True,True,True]]
--
map4nbs = mapnbs nbs4

-- |The 'map8los' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- A supplied predicate tells 'map8los' what is considered an empty cell, and
-- the mapping function is provided the cell value, and a list of cell values
-- that are in the line of sight of it in the four cardinal directions plus
-- the four intercardinal directions.
--
-- >>> conwayRule x ns = let n = count True ns in n == 3 || x && n == 2
-- >>> map8los not conwayRule $ ltov2 $ replicate 5 $ replicate 5 True
-- [[True,False,False,False,True],[False,False,False,False,False],[False,False,False,False,False],[False,False,False,False,False],[True,False,False,False,True]]
--
map8los = maplos nbs8

-- |The 'map4los' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- A supplied predicate tells 'map8los' what is considered an empty cell, and
-- the mapping function is provided the cell value, and a list of cell values
-- that are in the line of sight of it in the four cardinal directions.
--
-- >>> conwayRule x ns = let n = count True ns in n == 3 || x && n == 2
-- >>> map4los not conwayRule $ ltov2 $ replicate 5 $ replicate 5 True
-- [[True,True,True,True,True],[True,False,False,False,True],[True,False,False,False,True],[True,False,False,False,True],[True,True,True,True,True]]
--
map4los = maplos nbs4

-- ** Coordinate calculations

-- *** 2-D space

-- |'Num' instance for 2-tuples
instance (Num a, Num b) => Num (a, b) where
  (x, y) + (u, v) = (x + u, y + v)
  (x, y) * (u, v) = (x * u, y * v)
  negate (x, y) = (negate x, negate y)
  fromInteger x = (fromInteger x, 0)
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)

-- |'*$' provides scalar multiplication of a 2-tuple.
--
-- >>> 2 *$ (1, 4)
-- (2,8)
--
(*$) :: Int -> (Int, Int) -> (Int, Int)
n *$ (x, y) = (n * x, n * y)

-- |The 'dir' function converts a cardinal direction (e.g. \'E') into a unit
-- coordinate.
--
-- >>> dir 'E'
-- (1,0)
--
dir 'E' = ( 1,  0)
dir 'N' = ( 0,  1)
dir 'W' = (-1,  0)
dir 'S' = ( 0, -1)
dir _   = ( 0,  0)

-- |The 'rot' function rotates an (x, y) coordinate 90 degrees.
--
-- >>> rot (-3, 4)
-- (-4,-3)
--
rot (x, y) = (-y, x)

-- |The 'rotn' function rotates an (x, y) coordinate through n 90-degree
-- turns.
--
-- >>> rotn 2 (-3, 4)
-- (3,-4)
--
rotn 0 = id
rotn n = rot . rotn ((n - 1) `mod` 4)

-- |The 'manhattan' function provides a 2-tuple's Manhattan distance from the
-- origin.
--
-- >>> manhattan (-3, 4)
-- 7
--
manhattan (x, y) = abs x + abs y
