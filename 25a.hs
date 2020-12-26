import AOC
import Math.NumberTheory.Powers.Modular

main = interact $ f . map read

m = 20201227

f :: [Int] -> Int
f [pub1, pub2] = powModInt pub' priv m
  where
    pub' = if pub /= pub1 then pub1 else pub2
    (priv, pub) = head $ filter ((\p -> p == pub1 || p == pub2) . snd) $ zip [0..] $ iterate ((`rem` m) . (*7)) 1
