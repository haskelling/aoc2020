import AOC

main = interact $ f . rights . map (parse p)

p :: Parser (Int, Int, Char, String)
p = do
  low <- many1 digit
  char '-'
  high <- many1 digit
  char ' '
  c <- letter
  string ": "
  s <- many1 letter
  return $ (read low, read high, c, s)

f xs = count True $ map test xs

test (low, high, c, s) = let n = count c s
                         in  n >= low && n <= high
