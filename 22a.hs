import AOC

main = interactg f

turn :: ([Int], [Int]) -> [Int]
turn (c:cs, d:ds) = turn $ if c > d then (cs ++ [c, d], ds) else (cs, ds ++ [d, c])
turn ([], ds) = ds
turn (cs, []) = cs

f [_:p1, _:p2] = sum $ zipWith (*) [n,n-1..] cs'
  where
    cs = map read p1
    ds = map read p2
    cs' = turn (cs, ds)
    n = length cs'
