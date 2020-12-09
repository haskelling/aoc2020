import AOC
import Data.Maybe
import qualified Data.Map as M

main = interact $ f . rights . map (parse p)

type Bag = Int

p :: Parser (Bag, [(Int, Bag)])
p = do
  b <- bag
  string " contain "
  bs <- (string "no other bags" >> return []) <|> (bags `sepBy1` string ", ")
  char '.'
  return (b, bs)

bag :: Parser Bag
bag = do
  d1 <- many1 letter
  char ' '
  d2 <- many1 letter
  string " bag"
  optional $ char 's'
  return $ hash $ d1 ++ ' ':d2

bags :: Parser (Int, Bag)
bags = do
  n <- read <$> many1 digit
  char ' '
  b <- bag
  return (n, b)

summarize :: Ord n => [(n, [(v, n)])] -> v -> ([(v, v)] -> v) -> n -> v
summarize dag v0 f n = head $ catMaybes $ map (M.!? n) $ iterate getAll M.empty
  where
    containedByAll xs = filter (matchesAll xs) dag
    matchesAll xs (_, ys) = null . (\\ xs) $ map snd ys

    getAll m = foldl' add m $ containedByAll $ M.keys m

    add m (b, ys) = M.insertWith (flip const) b (calc m ys) m
    calc _ [] = v0
    calc m xs = f $ map (fmap (m M.!)) xs

f bs = summarize bs 0 (sum . (map (\(i, v) -> i * (v + 1)))) $ hash "shiny gold"
