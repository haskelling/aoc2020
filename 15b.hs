import AOC
import Control.Monad.ST
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as V

main = interact' $ f . map read . splitOn ","

nn = 30000000

f :: [Int] -> Int
f xs = get nn
  where
    l = length xs
    get i = if i < l then xs !! i else get' i
    get' target = runST $ do
      let target' = target - l
          y = last xs
          v0 = zip (init xs) [1..]
      v <- V.new nn
      forM_ v0 $ uncurry $ V.write v
      stepM target' y l v
    stepM 0 y _ _ = return y
    stepM target' y i v = do
      n <- V.read v y
      let y' = if n == 0 then 0 else i - n
      V.write v y i
      stepM (target' - 1) y' (i + 1) v
