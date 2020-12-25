import AOC

main = interact $ f . parselist (many1 enump)

data Dir = E | SE | SW | W | NW | NE deriving (Show, Eq, Read, Ord, Bounded, Enum)

godir E  (x, y) = (x + 1, y)
godir W  (x, y) = (x - 1, y)
godir NE (x, y) = (x + 1, y + 1)
godir NW (x, y) = (x, y + 1)
godir SE (x, y) = (x, y - 1)
godir SW (x, y) = (x - 1, y - 1)

nbs = map (`godir` 0) [E .. NE]
rule n ns = let c = count True ns in if n then not (c == 0 || c > 2) else c == 2

coordsToMap margin zs = [[(x, y) `elem` zs | x <- [minx-margin..maxx+margin]]
                                           | y <- [miny-margin..maxy+margin]]
  where
    (minx, miny) = minimum zs
    (maxx, maxy) = maximum zs

nIters = 100

f xs = sum $ map (count True) $ vtol2 $ applyN nIters (mapnbs nbs rule) $ ltov2 m
  where
    m = coordsToMap nIters $ map head $ filter (odd . length) $ group $ sort $ map (foldl' (flip godir) 0) xs
