import AOC

main = interact f

m s (x:xs) = m' s x (read xs :: Int)
  where
    m' (x, v) 'F' n = (x + (n *$ v), v)
    m' (x, v) 'L' n = (x, rotn (n `div` 90) v)
    m' (x, v) 'R' n = m' (x, v) 'L' (-n)
    m' (x, v) v'  n = (x + (n *$ dir v'), v)

f xs = manhattan $ fst $ foldl' m (0, dir 'E') xs
