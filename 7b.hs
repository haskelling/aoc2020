import AOC
import qualified Data.IntMap as M

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

converge :: Eq a => (a -> a) -> a -> a
converge f x = let x' = f x in if x' == x then x else converge f x'

f :: [(Bag, [(Int, Bag)])] -> Int
f bs = converge getAll M.empty M.! hash "shiny gold"
  where
    containedByAll bs' = filter (matchesAll bs') bs
    matchesAll xs (_, ys) = null . (\\ xs) $ map snd ys

    getAll m = foldl' add m $ containedByAll $ M.keys m

    add m (b, ys) = M.insertWith (flip const) b (calc m ys) m
    calc m xs = sum $ map (\(i, b') -> i * (1 + m M.! b')) xs
