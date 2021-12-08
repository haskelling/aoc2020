{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}
{-|
Module      : AOC
Description : A library of useful functions for solving coding problems
Copyright   : (c) 2021 Haskell Coder
License     : GPL-3
Maintainer  : haskelling@pdr.cx
Stability   : experimental
Portability : POSIX

AOC is a library of useful functions for solving coding problems.

It accompanies a [video series](https://www.youtube.com/playlist?list=PLDRIsR-OaZkzqqyss1B01G_7RWuXnUeb5)
describing how to solve the [2021 Advent of Code challenges](https://www.adventofcode.com/2021).
It is intended as a learning tool for beginner and intermediate Haskellers, so
often goes into some depth describing Haskell concepts, tools, and library
functions.

The code for this module along with the 2021 AoC solutions can be cloned from [GitHub](https://github.com/haskelling/aoc2021).
-}
module AOC(module Prelude, module AOC, module Text.Parsec, module Data.Vector, module Data.Char, module Data.List, module Data.List.Split, module Data.List.Extra, module Data.Hashable, module Data.Maybe, module Data.Either, module Data.Bool, module Control.Monad, module Text.Parsec.Expr, module Control.Arrow) where

import           Control.Arrow
import           Control.Exception     (ArithException (..))
import           Control.Monad
import           Data.Bits
import           Data.Bool
import           Data.Char
import           Data.Either
import           Data.Hashable         (hash)
import           Data.IntMap           (IntMap)
import qualified Data.IntMap           as IM
import           Data.List             hiding (nub)
import           Data.List.Split       hiding (endBy, oneOf, sepBy)
import           Data.List.Extra       hiding (splitOn, merge, chunksOf, linesBy, wordsBy, lower, upper, split, nub)
import qualified Data.Map              as M
import           Data.Map.Merge.Strict
import qualified Data.Map.Strict       as MS
import           Data.Maybe
import qualified Data.Set              as S
import           Data.Tuple
import           Data.Vector           (Vector, imap)
import qualified Data.Vector           as V
import           Data.Vector.Unboxed   (Unbox)
import qualified Data.Vector.Unboxed   as U
import           Numeric
import           Prelude               hiding (interact)
import qualified Prelude
import           Text.Parsec           hiding (count, parse, uncons)
import qualified Text.Parsec           as Parsec
import           Text.Parsec.Expr


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


-- * Parsing, Reading and Showing

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
-- Any non-binary digit encountered will result in a Nothing.
--
-- >>> readBin "01011010"
-- Just 90
--
readBin :: ReadBin a => [a] -> Maybe Int
readBin = foldl' add (Just 0)
  where
    add x y = do
      x' <- x
      y' <- toBin y
      return $ x' * 2 + y'

class ReadBin a where
  toBin :: a -> Maybe Int

instance ReadBin Char where
  toBin '0' = Just 0
  toBin '1' = Just 1
  toBin  _  = Nothing

instance ReadBin Bool where
  toBin False = Just 0
  toBin True  = Just 1

newtype Bin = Bin { unBin :: Int } deriving (Eq, Ord, Enum, Bounded, Num, Bits)

instance Show Bin where
  showsPrec _ (Bin i) = showIntAtBase 2 intToDigit i

instance Read Bin where
  readsPrec _ s = let (x, y) = span (`elem` "01") s in [(Bin $ fromJust $ readBin x, y)]

-- ** Showing

-- |A newtype for String whose show implementation doesn't add quotes.
--
-- >>> Str "abc"
-- abc
--
newtype Str = Str String deriving (Eq, Ord, Read)
instance Show Str where
  show (Str s) = s


-- * List functions

-- | The 'count' function returns the number of occurrences of the given value
-- in the given list.
--
-- >>> count 'e' "Advent of Code 2021"
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

-- | The 'ltov3' function converts a list of lists of lists into a 'Vector' of 'Vector's of 'Vector's.
ltov3 :: [[[a]]] -> Vector (Vector (Vector a))
ltov3 = ltov . map ltov2

-- | The 'ltov4' function converts a list of lists of lists of lists into a 'Vector' of 'Vector's of 'Vector's of 'Vector's.
ltov4 :: [[[[a]]]] -> Vector (Vector (Vector (Vector a)))
ltov4 = ltov . map ltov3

-- | The 'vtol' function is a convenience function for 'Vector.toList'.
vtol :: Vector a -> [a]
vtol = V.toList

-- | The 'vtol2' function converts a 'Vector' of 'Vector's into a list of lists.
vtol2 :: Vector (Vector a) -> [[a]]
vtol2 = map vtol . vtol

-- | The 'vtol2' function converts a 'Vector' of 'Vector's of 'Vector's into a list of lists of lists.
vtol3 :: Vector (Vector (Vector a)) -> [[[a]]]
vtol3 = map vtol2 . vtol

-- | The 'vtol2' function converts a 'Vector' of 'Vector's of 'Vector's of 'Vector's into a list of lists of lists of lists.
vtol4 :: Vector (Vector (Vector (Vector a))) -> [[[[a]]]]
vtol4 = map vtol3 . vtol


-- * Algorithms

-- ** Directed Acyclic Graph Algorithms

-- |The 'summarize' function does a bottom-up calculation on a directed
-- acyclic graph (DAG) by running a summary function on each node.
--
-- >>> f [] = 1; f xs = sum $ map snd xs
-- >>> summarize ([1..10], \x -> [(0, y) | y <- [2*x, 3*x..10]]) f 1
-- 13
--
-- >>> f [] = 1; f xs = sum $ map (\(w, s) -> w * s) xs
-- >>> summarize ([1..10], \x -> [(y, y) | y <- [2*x, 3*x..10]]) f 1
-- 279
--
summarize :: Ord n
          => ([n], n -> [(v, n)]) -- ^ The list of nodes and function from node to list of children, given as a tuple of edge "weight" and child node
          -> ([(v, w)] -> w)      -- ^ Summary function to populate the summary value for each node given a list of tuples representing the edge "weight" and summary value for each child node
          -> n                    -- ^ The node for which we need the summary
          -> w                    -- ^ The resulting summary value
summarize (nodes, children) f = (m M.!)
  where
    m = M.fromSet (f . map2 (m M.!) . children) $ S.fromList nodes

-- ** Maths Functions

discreteLog :: Int -> Int -> Int -> Int
discreteLog n x m = p - (xs IM.! k)
  where
    s = ceiling (sqrt (fromIntegral m) :: Double)
    xs = IM.fromList [(((x `mod` m) * expMod n r m) `mod` m, r) | r <- [0 .. s]]
    ys = IM.fromList [(expMod n ((s -1) * r) m, (s -1) * r) | r <- [1 .. s]]
    (k, p) = head $ IM.toList $ IM.filterWithKey (\k_ _ -> IM.member k_ xs) ys

expMod :: Int -> Int -> Int -> Int
expMod 0 _ _ = 0
expMod _ 0 _ = 1
expMod x e m
  | even e = let p = expMod x (e `div` 2) m in mo $! p * p
  | otherwise = mo $! x * expMod x (e - 1) m
  where mo = flip mod m

-- ** General-Purpose Functions

-- | Replaces the 'nub' function from Data.List with a faster version that also sorts the list.
nub :: Ord a => [a] -> [a]
nub = map head . group . sort

-- | The 'map2' function is simply 'fmap' '.' 'fmap'.
map2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
map2 = fmap . fmap

-- |The 'converge' function repeatedly applies f until there's no change
-- in the output.  That is, it calculates \( f (f (f ... (f x))) \).
converge :: Eq a => (a -> a) -> a -> a
converge f x = let x' = f x in if x' == x then x else converge f x'

-- |The 'applyN' function applies f n times.
--
-- >>> applyN 5 (+2) 3
-- 13
--
applyN n = foldr (.) id . replicate n

-- ** Grid Algorithms

type GridUV2 a = (Int, U.Vector a)

ltouv2 :: Unbox a => [[a]] -> GridUV2 a
ltouv2 = length . head &&& U.fromList . concat

uvtol2 :: Unbox a => GridUV2 a -> [[a]]
uvtol2 (w, v) = chunksOf w $ U.toList v

mapnbsuv :: (Unbox a, Unbox b)
         => [(Int, Int)]     -- ^ The list of coordinate offsets
         -> (a -> [a] -> b)  -- ^ The mapping function
         -> GridUV2 a        -- ^ The original grid
         -> GridUV2 b        -- ^ The updated grid
mapnbsuv nbs f (w, v) = (w, U.fromList $ modify 0 0 $ U.toList v)
  where
    modify _ _ [] = []
    modify i j xs | i == w = modify 0 (j + 1) xs
    modify i j (x:xs) = f x (mapMaybe (get (i, j)) nbs):modify (i + 1) j xs
    get z0 z = if x' < 0 || x' >= w || y' < 0 then Nothing else v U.!? (y' * w + x')
      where
        (x', y') = z0 + z

type Grid2 a = MS.Map (Int, Int) a

ltog2 :: [[a]] -> Grid2 a
ltog2 = MS.fromList . concat . zipWith (\j -> zipWith (\i -> ((i, j),)) [0..]) [0..]

gtol2 :: a -> Grid2 a -> [[a]]
gtol2 d m = [[fromMaybe d (m MS.!? (x, y)) | x <- [minimum xs .. maximum xs]] | y <- [minimum ys .. maximum ys]]
  where
    (xs, ys) = unzip $ MS.keys m

type Grid3 a = MS.Map (Int, Int, Int) a

ltog3 :: [[[a]]] -> Grid3 a
ltog3 = MS.fromList . concat . zipWith (\k -> concat . zipWith (\j -> zipWith (\i -> ((i, j, k),)) [0..]) [0..]) [0..]

gtol3 :: a -> Grid3 a -> [[[a]]]
gtol3 d m = [[[fromMaybe d (m MS.!? (x, y, z)) | x <- [minimum xs .. maximum xs]] | y <- [minimum ys .. maximum ys]] | z <- [minimum zs .. maximum zs]]
  where
    (xs, ys, zs) = unzip3 $ MS.keys m

type Grid4 a = MS.Map (Int, Int, Int, Int) a

ltog4 :: [[[[a]]]] -> Grid4 a
ltog4 = MS.fromList . concat . zipWith (\l -> concat . zipWith (\k -> concat . zipWith (\j -> zipWith (\i -> ((i, j, k, l),)) [0..]) [0..]) [0..]) [0..]

gtol4 :: a -> Grid4 a -> [[[[a]]]]
gtol4 d m = [[[[fromMaybe d (m MS.!? (w, x, y, z)) | w <- [minimum ws .. maximum ws]] | x <- [minimum xs .. maximum xs]]
                                                   | y <- [minimum ys .. maximum ys]] | z <- [minimum zs .. maximum zs]]
  where
    (ws, xs, ys, zs) = unzip4 $ MS.keys m

mapnbsg :: (Eq a, Num ix, Ord ix)
        => [ix]             -- ^ The list of coordinate offsets
        -> (a -> [a] -> a)  -- ^ The mapping function
        -> a                -- ^ The value of an empty cell
        -> MS.Map ix a      -- ^ The original grid
        -> MS.Map ix a      -- ^ The updated grid
mapnbsg nbs f d m = merge (mapMaybeMissing     $ \_ x -> f' x [])
                          (mapMaybeMissing     $ const $ f' d)
                          (zipWithMaybeMatched $ const f')
                          m m'
  where
    m' = M.fromListWith (++) [(x + n, [v]) | (x, v) <- M.toList m, n <- nbs]
    f' x xs = let x' = f x xs in if x' == d then Nothing else Just x'

mapnbsv :: [(Int, Int)]       -- ^ The list of coordinate offsets
        -> (a -> [a] -> b)    -- ^ The mapping function
        -> Vector (Vector a)  -- ^ The original grid
        -> Vector (Vector b)  -- ^ The updated grid
mapnbsv nbs f m = imap (\y v -> imap (\x i -> modify i (x, y)) v) m
  where
    modify i x = f i $ mapMaybe (get x) nbs
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)

-- |The 'mapnbs' function maps a function over a 'List' of 'List's,
-- treating it as a grid of cells.
-- Given a list of coordinate offsets, the mapping function is provided the
-- cell value, and a list of the values of the cells at those offsets.
--
-- >>> mapnbs nbs8 conwayRule False $ replicate 5 $ replicate 5 True
-- [[False,False,True,True,True,False,False],[False,True,False,False,False,True,False],[True,False,False,False,False,False,True],[True,False,False,False,False,False,True],[True,False,False,False,False,False,True],[False,True,False,False,False,True,False],[False,False,True,True,True,False,False]]
--
-- >>> mapnbs nbs4 conwayRule False $ replicate 5 $ replicate 5 True
-- [[True,True,True,True,True],[True,False,False,False,True],[True,False,False,False,True],[True,False,False,False,True],[True,True,True,True,True]]
--
mapnbs :: Eq a
       => [(Int, Int)]    -- ^ The list of coordinate offsets
       -> (a -> [a] -> a) -- ^ The mapping function
       -> a               -- ^ The value of an empty cell
       -> [[a]]           -- ^ The original grid
       -> [[a]]           -- ^ The updated grid
mapnbs nbs f d = gtol2 d . mapnbsg nbs f d . ltog2

mapnbsC :: (Unbox a, Eq a)
        => [(Int, Int)]    -- ^ The list of coordinate offsets
        -> (a -> [a] -> a) -- ^ The mapping function
        -> [[a]]           -- ^ The original grid
        -> [[a]]           -- ^ The updated grid
mapnbsC nbs f = uvtol2 . converge (mapnbsuv nbs f) . ltouv2

mapnbsN :: Eq a
        => Int             -- ^ The number of iterations
        -> [(Int, Int)]    -- ^ The list of coordinate offsets
        -> (a -> [a] -> a) -- ^ The mapping function
        -> a               -- ^ The value of an empty cell
        -> [[a]]           -- ^ The original grid
        -> Grid2 a         -- ^ The updated grid
mapnbsN n nbs f d = applyN n (mapnbsg nbs f d) . ltog2

-- |The 'mapnbs3' function is the 3-d variant of 'mapnbs'.
mapnbs3 :: Eq a
        => [(Int, Int, Int)]  -- ^ The list of coordinate offsets
        -> (a -> [a] -> a)    -- ^ The mapping function
        -> a                  -- ^ The value of an empty cell
        -> [[[a]]]            -- ^ The original grid
        -> Grid3 a            -- ^ The updated grid
mapnbs3 nbs f d = mapnbsg nbs f d . ltog3

mapnbs3N :: Eq a
         => Int                -- ^ The number of iterations
         -> [(Int, Int, Int)]  -- ^ The list of coordinate offsets
         -> (a -> [a] -> a)    -- ^ The mapping function
         -> a                  -- ^ The value of an empty cell
         -> [[[a]]]            -- ^ The original grid
         -> Grid3 a            -- ^ The updated grid
mapnbs3N n nbs f d = applyN n (mapnbsg nbs f d) . ltog3

-- |The 'mapnbs4' function is the 4-d variant of 'mapnbs'.
mapnbs4 :: Eq a
        => [(Int, Int, Int, Int)]  -- ^ The list of coordinate offsets
        -> (a -> [a] -> a)         -- ^ The mapping function
        -> a                       -- ^ The value of an empty cell
        -> [[[[a]]]]               -- ^ The original grid
        -> Grid4 a                 -- ^ The updated grid
mapnbs4 nbs f d = mapnbsg nbs f d . ltog4

mapnbs4N :: Eq a
         => Int                     -- ^ The number of iterations
         -> [(Int, Int, Int, Int)]  -- ^ The list of coordinate offsets
         -> (a -> [a] -> a)         -- ^ The mapping function
         -> a                       -- ^ The value of an empty cell
         -> [[[[a]]]]               -- ^ The original grid
         -> Grid4 a                 -- ^ The updated grid
mapnbs4N n nbs f d = applyN n (mapnbsg nbs f d) . ltog4

elems = MS.elems

-- |The 'maplos' function maps a function over a 'Vector' of 'Vector's,
-- treating it as a grid of cells.
-- A coordinate list is supplied and the given predicate determines what is
-- considered an empty cell.  The mapping function is provided the cell value,
-- and a list of cell values that are in the line of sight, in the direction of
-- the given coordinates.
--
-- >>> maplos nbs8 not conwayRule $ ltov2 $ replicate 5 $ replicate 5 True
-- [[True,False,False,False,True],[False,False,False,False,False],[False,False,False,False,False],[False,False,False,False,False],[True,False,False,False,True]]
--
-- >>> maplos nbs4 not conwayRule $ ltov2 $ replicate 5 $ replicate 5 True
-- [[True,True,True,True,True],[True,False,False,False,True],[True,False,False,False,True],[True,False,False,False,True],[True,True,True,True,True]]
--
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

maplosC :: Eq a
        => [(Int, Int)]    -- ^ The list of coordinates indicating the line-of-sight directions
        -> (a -> Bool)     -- ^ The isEmpty predicate - line of sight continues through cells whose value returns 'True'
        -> (a -> [a] -> a) -- ^ The mapping function, given the cell value and the values of all cells found through a line-of-sight search
        -> [[a]]           -- ^ The original grid
        -> [[a]]           -- ^ The updated grid
maplosC nbs p f m = vtol2 $ converge (maplos nbs p f) $ ltov2 m

nbs4, nbs8 :: [(Int, Int)]
-- |'nbs4' lists the offsets of the four non-diagonal neighbours of a cell
-- in a grid.
nbs4 = [(0, 1), (-1, 0), (1, 0), (0, -1)]
-- |'nbs8' lists the offsets of the eight neighbours of a cell in a grid.
nbs8 = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

nbs6, nbs26 :: [(Int, Int, Int)]
-- |3-d equivalent of 'nbs4'
nbs6 = [(0, 0, 1), (0, -1, 0), (0, 1, 0), (0, 0, -1), (1, 0, 0), (-1, 0, 0)]
-- |3-d equivalent of 'nbs8'
nbs26 = [(x, y, z) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x, y, z) /= (0, 0, 0)]

nbs8_4, nbs80 :: [(Int, Int, Int, Int)]
-- |3-d equivalent of 'nbs4'
nbs8_4 = [(0, 0, 0, 1), (0, 0, -1, 0), (0, 0, 1, 0), (0, 0, 0, -1),
          (0, 1, 0, 0), (0, -1, 0, 0), (1, 0, 0, 0), (-1, 0, 0, 0)]
-- |3-d equivalent of 'nbs8'
nbs80 = [(x, y, z, w) | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], (x, y, z, w) /= (0, 0, 0, 0)]

-- |The 'conwayRule' function is an implementation of the rule followed by the
-- cells in Conway's Game of Life.  It can be used with the 'map8nbs' function
-- to model the original version of the game.
--
-- >>> conwayRule True [True, False, True, False, True, True, False, False]
-- False
--
conwayRule :: Bool   -- ^ The cell's current state
           -> [Bool] -- ^ The current states of the cell's neighbours
           -> Bool   -- ^ The cell's next state
conwayRule x ns = let n = count True ns in n == 3 || x && n == 2

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

-- *** 3-D space

-- |'Num' instance for 3-tuples
instance (Num a, Num b, Num c) => Num (a, b, c) where
  (x, y, z) + (u, v, w) = (x + u, y + v, z + w)
  (x, y, z) * (u, v, w) = (x * u, y * v, z * w)
  negate (x, y, z) = (negate x, negate y, negate z)
  fromInteger x = (fromInteger x, 0, 0)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)

t1 (x, _, _) = x
t2 (_, x, _) = x
t3 (_, _, x) = x

-- *** 4-D space

-- |'Num' instance for 4-tuples
instance (Num a, Num b, Num c, Num d) => Num (a, b, c, d) where
  (w, x, y, z) + (h, i, j, k) = (w + h, x + i, y + j, z + k)
  (w, x, y, z) * (h, i, j, k) = (w * h, x * i, y * j, z * k)
  negate (w, x, y, z) = (negate w, negate x, negate y, negate z)
  fromInteger x = (fromInteger x, 0, 0, 0)
  abs (w, x, y, z) = (abs w, abs x, abs y, abs z)
  signum (w, x, y, z) = (signum w, signum x, signum y, signum z)

s1 (x, _, _, _) = x
s2 (_, x, _, _) = x
s3 (_, _, x, _) = x
s4 (_, _, _, x) = x

-- |'Num' instance for 'Maybe' 'Num's
instance Num a => Num (Maybe a) where
  x * y       = (*) <$> x <*> y
  x + y       = (+) <$> x <*> y
  abs         = (abs <$>)
  signum      = (signum <$>)
  fromInteger = (Just . fromInteger)
  negate      = (negate <$>)
