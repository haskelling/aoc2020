import AOC

main = interact $ f . map (map (=='#'))

f m = countActive $ vtol4 $ applyN n (map80nbs conwayRule) $ ltov4 start
  where
    n = 6
    l = length m
    countActive = sum . map (sum . map (sum . map (count True)))
    start = expandWith blankcube [expandWith blankplane [expandWith blankrow $ map (expandWith False) m]]
    expandWith x y = replicate n x ++ y ++ replicate n x
    blankrow = replicate (n * 2 + l) False
    blankplane = replicate (n * 2 + l) blankrow
    blankcube = replicate (n * 2 + l) blankplane
