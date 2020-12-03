import AOC

main = interact $ f' . map read

f' (x:xs) = case f (2020 - x) xs of
              Nothing -> f' xs
              Just y  -> y * x

f n (x:xs) = if (n - x) `elem` xs then Just (x * (n - x)) else f n xs
f _ [] = Nothing
