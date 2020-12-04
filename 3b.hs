import AOC

main = interact $ f . map (ltov . map (=='#'))

f m = product $ map (f'' m) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

f'' m (v, w) = count True $ f' m (v, w - 1) 0 0

f' (m:ms) (v, w) x 0 = (m !| x) : f' ms (v, w) (x + v) w
f' (_:ms) (v, w) x j = f' ms (v, w) x (j - 1)
f' _ _ _ _ = []
