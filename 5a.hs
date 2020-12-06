import AOC

main = interact $ maximum . map (readBin .  tr "FBLR" "0101")
