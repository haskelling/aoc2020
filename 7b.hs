import AOC
import Data.Maybe
import qualified Data.Map as M

main = interact $ f . parselist p

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

f bs = summarize (map fst bs, (M.fromList bs M.!)) (sum . map (\(i, v) -> i * (v + 1))) $ hash "shiny gold"
