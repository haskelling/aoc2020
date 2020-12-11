import AOC

main = interact $ f . map read

f :: [Int] -> Int
f xs = f' (take 25 xs) (drop 25 xs)

f' (x:xs) (y:ys) = if t y (x:xs) then f' (xs ++ [y]) ys else y

t y (x:xs) = y - x `elem` xs || t y xs
t y [] = False
