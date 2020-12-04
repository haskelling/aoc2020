import AOC
import Data.Char
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

f (Right ps) = count True . map isValid . filter ((==7) . M.size) . map (M.delete Cid . M.fromList) $ ps

hgtp :: Parser (Int, String)
hgtp = do
  h <- many1 digit
  u <- many1 anyChar
  return (read h, u)

isValid p =
  let
    byr = read (p M.! Byr) :: Int
    iyr = read (p M.! Iyr) :: Int
    eyr = read (p M.! Eyr) :: Int
    hgt = parse hgtp $ p M.! Hgt
    hgtok = case hgt of
      Left _ -> False
      Right (hgt, hgtu) -> case hgtu of
        "in" -> hgt >= 59 && hgt <= 76
        "cm" -> hgt >= 150 && hgt <= 193
        _ -> False
    (h1:hcl) = p M.! Hcl
    ecl = p M.! Ecl
    pid = p M.! Pid
  in
    and [ byr >= 1920, byr <= 2002
        , iyr >= 2010, iyr <= 2020
        , eyr >= 2020, eyr <= 2030
        , hgtok
        , h1 == '#', length hcl == 6, all (`elem` "0123456789abcdef") hcl
        , ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        , length pid == 9, all isDigit pid
        ]
