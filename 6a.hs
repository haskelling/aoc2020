import AOC

main = interactg $ sum . map (length . nub . concat)
