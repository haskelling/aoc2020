import Prelude

main = interact $ (++"\n") . show . f . map (read :: String -> Int) . lines

f :: [Int] -> Int
f (x:xs) = if (2020 - x) `elem` xs then x * (2020 - x) else f xs
