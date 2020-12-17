import AOC
import Data.Bool
import qualified Data.Vector as V

main = interact $ f . map (map (=='#'))

showMap = unlines . map (map (bool '.' '#'))

mapnbs4 nbs f m = imap (\w c -> imap (\z p -> imap (\y v -> imap (\x i -> modify i (x, y, z, w)) v) p) c) m
  where
    modify i x = f i $ mapMaybe (get x) nbs
    get (x0, y0, z0, w0) (x, y, z, w) = do
      cube <- m V.!? (w0 + w)
      plane <- cube V.!? (z0 + z)
      row <- plane V.!? (y0 + y)
      row V.!? (x0 + x)

nbs80 = [(x, y, z, w) | x <- [-1..1], y <- [-1..1], z <- [-1..1], w <- [-1..1], (x, y, z, w) /= (0, 0, 0, 0)]
mapnbs80 = mapnbs4 nbs80
conwayRule x ns = let n = count True ns in n == 3 || x && n == 2

ltov3 = ltov . map ltov2
vtol3 = map vtol2 . vtol
ltov4 = ltov . map ltov3
vtol4 = map vtol3 . vtol

applyN n = foldr1 (.) . replicate n

f m = countActive $ vtol4 $ applyN n (mapnbs80 conwayRule) $ ltov4 start
  where
    n = 6
    l = length m
    countActive = sum . map (sum . map (sum . map (count True)))
    start = expandWith blankcube [expandWith blankplane [expandWith blankrow $ map (expandWith False) m]]
    expandWith x y = replicate n x ++ y ++ replicate n x
    blankrow = replicate (n * 2 + l) False
    blankplane = replicate (n * 2 + l) blankrow
    blankcube = replicate (n * 2 + l) blankplane
