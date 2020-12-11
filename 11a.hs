import AOC
import Data.Bool
import qualified Data.Vector as V

main = interact $ f . ltov . map (ltov . map (bool 0 1 . (=='L')))

map8nbs :: (a -> [a] -> b) -> Vector (Vector a) -> Vector (Vector b)
map8nbs f m = V.imap (\y v -> V.imap (\x i -> modify i (x, y)) v) m
  where
    modify i (x, y) = f i $ mapMaybe (get (x, y)) nbs
    get (x0, y0) (x, y) = do
      row <- m V.!? (y0 + y)
      row V.!? (x0 + x)
    nbs = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

step = map8nbs g
  where
    g x ns = case x of
      1 -> if count 2 ns == 0 then 2 else 1
      2 -> if count 2 ns >= 4 then 1 else 2
      x -> x

f m = sum $ map (count 2 . vtol) $ vtol $ converge step m
