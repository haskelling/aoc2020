import AOC

main = interact f

data Mod = Mod Integer Integer deriving (Show, Read, Eq, Ord)

instance Semigroup Mod where (<>) = crt
instance Monoid Mod where mempty = Mod 0 1

crt (Mod x m) (Mod y n) =
  let t = modinv (Mod n m) * x * n + modinv (Mod m n) * y * m
      m' = m * n
      x' = t `mod` m'
  in  Mod x' m'
  where
    modinv (Mod x m) = let (_, x', _) = exteuc (Mod x m) in x' `mod` m
    exteuc (Mod 0 m) = (m, 0, 1)
    exteuc (Mod x m) = let (g, y, x') = exteuc (Mod (m `mod` x) x)
                       in  (g, x' - m `div` x * y, y)

f [_, bs] = let (Mod x m) = mconcat (fst bs') in ((m - x) `mod` m)
  where
    bs' = foldl' g ([], 0) $ map readBus $ splitOn "," bs
    g (xs, j) Nothing = (xs, j + 1)
    g (xs, j) (Just n) = (xs ++ [Mod j n], j + 1)
    readBus "x" = Nothing
    readBus s = Just $ read s
