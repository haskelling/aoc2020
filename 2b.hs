import AOC

main = interact $ f . parselist p

p :: Parser (Int, Int, Char, String)
p = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return (read low, read high, c, s)

f xs = count True $ map test xs

test (low, high, c, s) = let p1 = s !! (low - 1)
                             p2 = s !! (high - 1)
                         in  (c == p1) /= (c == p2)
