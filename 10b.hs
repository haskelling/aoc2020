import AOC

main = interact $ f . map read

f :: [Int] -> Int
f xs = summarize (all, getChildren) 1 calc 0
  where
    all = 0:sort xs ++ [deviceRating]
    deviceRating = maximum xs + 3
    getChildren j = [(1, k) | k <- [j + 1 .. j + 3], k `elem` all]
    calc = sum . map snd
