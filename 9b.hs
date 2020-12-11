import AOC

main = interact $ g . map read

g :: [Int] -> Int
g xs = let n = f xs
           xs' = h n xs
       in maximum xs' + minimum xs'

h n xs = head $ filter ((==n) . sum) $ concatMap tails $ inits xs

f xs = f' (take 25 xs) (drop 25 xs)

f' (x:xs) (y:ys) = if t y (x:xs) then f' (xs ++ [y]) ys else y

t y (x:xs) = y - x `elem` xs || t y xs
t y [] = False
