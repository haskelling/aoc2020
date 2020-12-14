import AOC

main = interact f

data Mod = Mod Integer Integer deriving (Show, Read, Eq, Ord)

crt (Mod x m) (Mod y n) =
  let t = modinv (Mod n m) * x * n + modinv (Mod m n) * y * m
      m' = m * n
      x' = t `rem` m'
  in  Mod x' m'
  where
    modinv (Mod x m) = let (_, x', _) = exteuc (Mod x m) in x' `rem` m
    exteuc (Mod 0 m) = (m, 0, 1)
    exteuc (Mod x m) = let (g, y, x') = exteuc (Mod (m `rem` x) x)
                       in  (g, x' - m `quot` x * y, y)

f [_, bs] = let (Mod x m) = foldl1' crt (fst bs') in ((m - x) `rem` m)
  where
    bs' = foldl' g ([], 0) $ map readBus $ splitOn "," bs
    g (xs, j) Nothing = (xs, j + 1)
    g (xs, j) (Just n) = (xs ++ [Mod j n], j + 1)
    readBus "x" = Nothing
    readBus s = Just $ read s
