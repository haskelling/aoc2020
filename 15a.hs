import AOC

main = interact' $ f . map read . splitOn ","

nn = 2020

f :: [Int] -> Int
f xs = head $ getList nn
  where
    getList n | n <= length xs = reverse $ take n xs
    getList n = let (y:ys) = getList (n - 1)
                    y' = if y `elem` ys
                           then 1 + length (takeWhile (/= y) ys)
                           else 0
                in  y':y:ys
