import AOC
import qualified Data.Map as M

main = interactg f

data Rule = Letter Char | And Rule Rule | Or Rule Rule | See Int deriving (Show, Eq, Read, Ord)

rulep :: String -> (Int, Rule)
rulep xs = (read $ init n, rs)
  where
    Right rs = parse rulep' $ unwords xs'
    (n:xs') = words xs
    rulep' = buildExpressionParser table term
    term = ((See <$> integer) <|> char '"' *> (Letter <$> anyChar) <* char '"') <* spaces
    table = [[Infix (spaces >> return And) AssocLeft], [Infix (char '|' >> spaces >> return Or) AssocLeft]]

mkParser :: M.Map Int Rule -> Rule -> Parser ()
mkParser _ (Letter c) = void $ char c
mkParser m (And x y) = mkParser m x >> mkParser m y
mkParser m (Or x y) = try (mkParser m x) <|> mkParser m y
mkParser m (See x) = mkParser m (m M.! x)

f [rs, ss] = count True $ map check ss
  where
    m = M.fromList $ map rulep rs
    p = do
      r42 <- many1 $ try $ mkParser m $ See 42
      r31 <- many1 $ mkParser m $ See 31
      guard $ length r42 > length r31
    check s = isRight $ parse (p >> eof) s
