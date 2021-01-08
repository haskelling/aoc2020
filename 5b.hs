import AOC

main = interact $ g . sort . mapMaybe (readBin . tr "FBLR" "0101")

g xs = head [succ x | (x, y) <- zip xs (tail xs), succ x /= y]
