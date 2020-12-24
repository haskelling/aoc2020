import AOC
import qualified Data.IntMap.Strict as IM
import Data.IntMap ((!))
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as V

main = interact $ f' . map digitToInt . head

nIters = 10000000
n = 1000000
dec 0 = n - 1
dec x = x - 1

h v current = do
  x1 <- V.read v current
  x2 <- V.read v x1
  x3 <- V.read v x2
  next <- V.read v x3
  let dec' x = if x == x1 || x == x2 || x == x3 then dec' $ dec x else x
      x = dec' $ dec current
  V.read v x >>= V.write v x3
  V.write v x x1
  V.write v current next
  return next

f' xs = runST $ do
  v <- V.new n
  zipWithM_ (V.write v) xs' (tail $ cycle xs')
  foldM_ (const . h v) (head xs') [1..nIters]
  r1 <- V.read v 0
  r2 <- V.read v r1
  return $ (r1+1) * (r2+1)
  where
    xs' = map (+ (-1)) xs ++ [length xs .. n - 1]

g (current, m) = g' $ dec' $ dec current
  where
    x1 = m ! current
    x2 = m ! x1
    x3 = m ! x2
    next = m ! x3
    dec' x = if x == x1 || x == x2 || x == x3 then dec' $ dec x else x
    g' x = (next, IM.insert x3 (m ! x) $ IM.insert x x1 $ IM.insert current next m)

f xs = r1 * r2
  where
    xs' = map (+ (-1)) xs ++ [length xs .. n - 1]
    xs'' = IM.fromList $ zip xs' (tail $ cycle xs')
    (r1:r2:_) = map (+1) $ mapToList 0 0 $ snd $ applyN nIters g (head xs', xs'')
    mapToList i0 i m = let i' = m ! i in if i' == i0 then [] else i' : mapToList i0 i' m
