import AOC
import qualified Data.Map as M

main = interact' $ f . parse p

data FT = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid deriving (Show, Eq, Ord, Enum, Bounded)

field :: Parser (FT, String)
field = do
  ft <- enump
  char ':'
  s <- manyTill anyChar space
  return (ft, s)

p :: Parser [[(FT, String)]]
p = many1 field `sepBy1` char '\n' <* eof

f (Right ps) = count 7 $ map (M.size . M.delete Cid . M.fromList) ps
