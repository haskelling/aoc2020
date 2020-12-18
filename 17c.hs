import AOC
import qualified Data.Vector as V
import qualified Data.Map as M

instance Num a => Num (Vector a) where
  x + y = V.zipWith (+) x y
  x * y = V.zipWith (*) x y
  abs = V.map abs
  signum = V.map signum
  fromInteger = V.singleton . fromInteger
  negate = V.map negate

main = interact $ f . map (map (=='#'))

dd = 4
nn = 6

mapnbs' :: [Vector Int] -> (Bool -> [Bool] -> Bool) -> M.Map (Vector Int) Bool -> M.Map (Vector Int) Bool
mapnbs' nbs f m = M.fromList $ filter snd $ map nextval ks'
  where
    nextval k = (k, f (get k) (map (get . (k+)) nbs))
    get k = fromMaybe False (m M.!? k)
    ks = M.keys m
    a = V.map (+ (-1)) $ foldl1' (V.zipWith min) ks  -- top-left-front-... corner of bounding box
    z = V.map (+1)     $ foldl1' (V.zipWith max) ks  -- bottom-right-back-... corner
    ks' = map V.fromList $ foldr (\x y -> (:) <$> x <*> y) [[]] $ V.zipWith (\x y -> [x..y]) a z

nbs n = delete (V.replicate n 0) (nbs' n)
  where
    nbs' 0 = [V.empty]
    nbs' n = let v = nbs' (n-1) in V.cons <$> [-1..1] <*> v

ltomap :: [[Bool]] -> M.Map (Vector Int) Bool
ltomap xss = M.fromList $ filter snd $ concatMap (\(j, row) -> zipWith (\i x -> (V.fromList (i:j:replicate (dd - 2) 0),x)) [0..] row) $ zip [0..] xss

f ts = countActive $ applyN nn domap $ ltomap ts
  where
    domap = mapnbs' (nbs dd) conwayRule
    countActive = count True . M.elems
