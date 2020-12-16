import AOC
import Control.Monad.ST
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.IntMap as M

main = interact' $ f . map read . splitOn ","

nn = 30000000

f :: [Int] -> Int
f xs = get nn
  where
    l = length xs
    get i = if i < l then xs !! (i - 1) else get' i
    get' target = runST $ do
      let target' = target - l
          y = last xs
      v <- V.new nn
      zipWithM_ (V.write v) (init xs) [1..]
      stepM target' y l v
    stepM 0 y _ _ = return y
    stepM target' y i v = do
      n <- V.read v y
      let y' = if n == 0 then 0 else i - n
      V.write v y i
      stepM (target' - 1) y' (i + 1) v

f' :: [Int] -> Int
f' xs = get nn
  where
    l = length xs
    get i = if i < l then xs !! (i - 1) else get' i
    get' target = step (target - l) (last xs) (l - 1) (M.fromList $ zip (init xs) [0..])
    step 0 y _ _ = y
    step target' y i m =
      let y' = case m M.!? y of
                 Just n  -> i - n
                 Nothing -> 0
      in  step (target' - 1) y' (i + 1) (M.insert y i m)
