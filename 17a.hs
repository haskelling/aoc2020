import AOC

main = interact $ f . map (map (=='#'))

f m = countActive $ vtol3 $ applyN n (map26nbs conwayRule) $ ltov3 start
  where
    n = 6
    l = length m
    countActive = sum . map (sum . map (count True))
    start = expandWith blankplane [expandWith blankrow $ map (expandWith False) m]
    expandWith x y = replicate n x ++ y ++ replicate n x
    blankrow = replicate (n * 2 + l) False
    blankplane = replicate (n * 2 + l) blankrow
