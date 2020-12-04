import AOC

main = interact $ f . map (ltov . map (=='#'))

f m = count True $ f' m 3 0

f' (m:ms) v x = m !| x : f' ms v (x + v)
f' _ _ _ = []
