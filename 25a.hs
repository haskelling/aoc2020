import AOC

main = interact $ f . map read

m = 20201227

f :: [Int] -> Int
f [pub1, pub2] = expMod pub1 priv m
  where
    priv = discreteLog 7 pub2 m
