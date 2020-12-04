import AOC
import qualified Data.Map as M

main = interact' $ f . parse p

data FT = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid deriving (Show, Eq, Ord)

fieldtype :: Parser FT
fieldtype = choice [sr "byr" Byr, sr "iyr" Iyr, sr "eyr" Eyr, sr "hgt" Hgt
                  , sr "hcl" Hcl, sr "ecl" Ecl, sr "pid" Pid, sr "cid" Cid]
  where
    sr :: String -> FT -> Parser FT
    sr s ft = try $ string s >> return ft

field :: Parser (FT, String)
field = do
  ft <- fieldtype
  char ':'
  s <- manyTill anyChar space
  return (ft, s)

p :: Parser [[(FT, String)]]
p = many1 $ do
  t <- many1 field
  (char '\n' >> return ()) <|> eof
  return t

f (Right ps) = count 7 $ map (length . M.delete Cid . M.fromList) ps
