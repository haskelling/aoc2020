import AOC
import Text.Parsec

main = interact $ f . rights . map (parse p "") . lines

rights (Right x:xs) = x:rights xs
rights (_:xs) = rights xs
rights [] = []

p :: Parsec String () (Int, Int, Char, String)
p = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return $ (read low, read high, c, s)

countelems c s = length $ filter (==c) s

f xs = countelems True $ map test xs

test (low, high, c, s) = let n = countelems c s
                         in  n >= low && n <= high
