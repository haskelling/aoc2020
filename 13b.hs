import AOC

main = interact f

-- Solution inspired by Uncle Bob: https://youtu.be/tHTDAUV-VsM?t=489
next :: Integer -> Integer -> [(Integer, Integer)] -> Integer
next i m [] = i
next i m bs@((n, x):bs') = if (i + n) `rem` x == 0 then next i (m*x) bs' else next (i + m) m bs

f [_, bs] = next 0 1 bs'
  where
    bs' = fst $ foldl' g ([], 0) $ map readBus $ splitOn "," bs
    g (xs, j) Nothing = (xs, j + 1)
    g (xs, j) (Just n) = (xs ++ [(j, n)], j + 1)
    readBus "x" = Nothing
    readBus s = Just $ read s
