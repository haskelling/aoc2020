import AOC
import Data.Bool
import qualified Data.Vector as V

main = interact $ f . map (map (=='#'))

showMap = unlines . map (map (bool '.' '#'))

mapnbs3 nbs f m = imap (\z p -> imap (\y v -> imap (\x i -> modify i (x, y, z)) v) p) m
  where
    modify i x = f i $ mapMaybe (get x) nbs
    get (x0, y0, z0) (x, y, z) = do
      plane <- m V.!? (z0 + z)
      row <- plane V.!? (y0 + y)
      row V.!? (x0 + x)

nbs26 = [(x, y, z) | x <- [-1..1], y <- [-1..1], z <- [-1..1], (x, y, z) /= (0, 0, 0)]
mapnbs26 = mapnbs3 nbs26
conwayRule x ns = let n = count True ns in n == 3 || x && n == 2

ltov3 = ltov . map ltov2
vtol3 = map vtol2 . vtol

applyN n = foldr1 (.) . replicate n

f m = countActive $ vtol3 $ applyN n (mapnbs26 conwayRule) $ ltov3 start
  where
    n = 6
    l = length m
    countActive = sum . map (sum . map (count True))
    start = expandWith blankplane [expandWith blankrow $ map (expandWith False) m]
    expandWith x y = replicate n x ++ y ++ replicate n x
    blankrow = replicate (n * 2 + l) False
    blankplane = replicate (n * 2 + l) blankrow
