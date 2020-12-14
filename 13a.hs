import AOC

main = interact f

f [ts, bs] = getResult $ minimum $ map busTime bs'
  where
    ts' = read ts
    bs' = mapMaybe readBus $ splitOn "," bs
    readBus "x" = Nothing
    readBus s = Just $ read s
    busTime x = (x - ts' `rem` x, x)
    getResult (t, b) = t * b
