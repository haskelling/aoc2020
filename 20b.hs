import AOC
import Data.Bits
import qualified Data.Map as M
import Data.List.Extra
import Data.Tuple.Extra

main = interactg f

data Orientation = Ori Bool Int deriving (Show, Eq, Read, Ord)

rotgrid = transpose . reverse
rotgridn n = applyN n rotgrid

orients = [Ori flipped nrots | flipped <- [False, True], nrots <- [0..3]]
orient (Ori False n) = rotgridn n
orient (Ori True  n) = rotgridn n . reverse

getorients g = [(o, orient o g) | o <- orients]

boolsToInt :: [Bool] -> Int
boolsToInt = foldl' (\x y -> x * 2 + bool 0 1 y) 0

monster = map (map (=='#')) ["                  # ", "#    ##    ##    ###", " #  #  #  #  #  #   "]
monsterSig = mkSig monster

mkSig :: [[Bool]] -> Int
mkSig ss = foldl (\a x -> a * 2 + bool 0 1 x) 0 $ concatMap (take 20) $ take 3 ss

findMonsters :: [[Bool]] -> Int
findMonsters ss = maximum $ map (uncurry findMonsters' . dupe . snd) $ getorients ss
  where
    findMonsters' ss0 ss = if length ss < 3
                            then 0
                            else
                              (if mkSig ss .&. monsterSig == monsterSig then 1 else 0) +
                                if length (head ss) > 20 then
                                  findMonsters' ss0 (map tail ss)
                                else
                                  let ss0' = tail ss0 in findMonsters' ss0' ss0'

g :: [String] -> (Int, [[Bool]])
g (t:s) = (read $ init t', s')
  where
    (_:t':_) = words t
    s' = map (map (=='#')) s

t2of3 (_,x,_) = x

showMap = unlines . map (map (bool '.' '#'))


f s = count True (concat completeGrid) - findMonsters completeGrid * count True (concat monster)
  where
    tiles = map g $ filter (not . null) s
    getInts (tnum, tile) = map (\(o, tile') -> (boolsToInt $ head tile', (o, tnum, tile'))) $ getorients tile
    tileIntMapping = concatMap getInts tiles
    uniqueEdges = filter ((==1) . length) $ groupOn fst $ sort tileIntMapping
    -- corner tiles are tiles with 4 unique edges (2 edges * 2 orientations)
    cornerTiles = filter ((==4) . length) $ groupOn t2of3 $ sortOn t2of3 $ map (snd . head) uniqueEdges

    tileMap = M.fromListWith (++) $ map (\(edge, (o, tnum, tile)) -> (edge, [(tnum, tile)])) tileIntMapping

    ((_,stNum,_):_) = head cornerTiles

    stOris = filter (\(Ori fl _, _, _) -> not fl) $ head cornerTiles
    stOri = let [(o1, t1), (o2, t2)] = map (\(Ori _ n, _, t) -> (n, t)) stOris
            in  if o2 == succ o1 `mod` 4 then t1 else t2
    stTile = (stNum, stOri)

    belowTiles (n, t) = case filter ((/=n) . fst) $ tileMap M.! boolsToInt (last t) of
                          [nexttile] -> (n, t):belowTiles nexttile
                          _          -> [(n, t)]
    rightTiles (n, t) = map (orient (Ori True 1) . snd) $ belowTiles (n, orient (Ori True 1) t)

    mid = tail . init
    allTiles = map (map (mid . map mid) . rightTiles) $ belowTiles stTile
    allTilesRows = map (foldl1' (zipWith (++))) allTiles
    completeGrid = concat allTilesRows
