import AOC

main = interact $ f . map read

f :: [Int] -> Int
f xs = get1diffs * get3diffs
  where
    all = 0:sort xs ++ [deviceRating]
    deviceRating = maximum xs + 3
    getdiffs = zipWith (-) (tail all) all
    get1diffs = count 1 getdiffs
    get3diffs = count 3 getdiffs
