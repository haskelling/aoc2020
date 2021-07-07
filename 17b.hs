import AOC

main = interact $ f . map (map (=='#'))

n = 6

f m = count True $ elems $ mapnbs4N n nbs80 conwayRule False [[m]]
