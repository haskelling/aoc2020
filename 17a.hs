import AOC

main = interact $ f . map (map (=='#'))

n = 6

f m = count True $ concat $ concat $ mapnbs3N n nbs26 conwayRule False [m]
