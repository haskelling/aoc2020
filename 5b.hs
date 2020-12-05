import AOC
import Data.List

main = interact $ g . sort . map f

f = foldl' (\x y -> x * 2 + y) 0 . map bin

bin 'F' = 0
bin 'B' = 1
bin 'L' = 0
bin 'R' = 1

g xs = head [succ x | (x, y) <- zip xs (tail xs), succ x /= y]
