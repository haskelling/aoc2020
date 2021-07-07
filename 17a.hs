import AOC

main = interact $ f . map (map (=='#'))

n = 6

f m = count True $ elems $ mapnbs3N n nbs26 conwayRule False [m]
