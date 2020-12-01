import Prelude

main = interact $ (++"\n") . show . f' . map (read :: String -> Int) . lines

f' :: [Int] -> Int
f' (x:xs) = case f (2020 - x) xs of
              Nothing -> f' xs
              Just y  -> y * x

f :: Int -> [Int] -> Maybe Int
f n (x:xs) = if (n - x) `elem` xs then Just (x * (n - x)) else f n xs
f _ [] = Nothing
