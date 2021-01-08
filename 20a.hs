import AOC
import Data.List.Extra

main = interactg f

data Orientation = Ori Bool Int deriving (Show, Eq, Read, Ord)

rotgrid = transpose . reverse
rotgridn n = applyN n rotgrid

orients = [Ori flipped nrots | flipped <- [False, True], nrots <- [0..3]]
orient (Ori False n) = rotgridn n
orient (Ori True  n) = rotgridn n . reverse

getorients g = [orient o g | o <- orients]

boolsToInt :: [Bool] -> Int
boolsToInt = fromJust . readBin

g :: [String] -> (Int, [[Bool]])
g (t:s) = (read $ init t', s')
  where
    (_:t':_) = words t
    s' = map (map (=='#')) s

f s = product cornerTiles
  where
    tiles = map g $ filter (not . null) s
    testtile = head tiles
    getInts (tnum, tile) = map ((,tnum) . boolsToInt . head) $ getorients tile
    tileIntMapping = concatMap getInts tiles
    uniqueEdges = filter ((==1) . length) $ groupOn fst $ sort tileIntMapping
    -- corner tiles are tiles with 4 unique edges (2 edges * 2 orientations)
    cornerTiles = map head $ filter ((==4) . length) $ group $ sort $ map (snd . head) uniqueEdges
