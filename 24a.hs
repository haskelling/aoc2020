import AOC

main = interact $ f . parselist (many1 enump)

data Dir = E | SE | SW | W | NW | NE deriving (Show, Eq, Read, Ord, Bounded, Enum)

godir E  (x, y) = (x + 1, y)
godir W  (x, y) = (x - 1, y)
godir NE (x, y) = (x + 1, y + 1)
godir NW (x, y) = (x, y + 1)
godir SE (x, y) = (x, y - 1)
godir SW (x, y) = (x - 1, y - 1)

f xs = length $ filter odd $ map length $ group $ sort $ map (foldl' (flip godir) 0) xs
