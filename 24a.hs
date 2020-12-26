import AOC

main = interact $ f . parselist (many1 enump)

data Dir = E | SE | SW | W | NW | NE deriving (Show, Eq, Read, Ord, Bounded, Enum)

dirToCoord E  = ( 1,  0)
dirToCoord W  = (-1,  0)
dirToCoord NE = ( 1,  1)
dirToCoord NW = ( 0,  1)
dirToCoord SE = ( 0, -1)
dirToCoord SW = (-1, -1)

godir z d = z + dirToCoord d

f xs = length $ filter odd $ map length $ group $ sort $ map (foldl' godir 0) xs
