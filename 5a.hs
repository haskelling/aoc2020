import AOC

main = interact $ maximum . mapMaybe (readBin .  tr "FBLR" "0101")
