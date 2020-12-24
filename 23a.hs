import AOC
import Data.Sequence
import qualified Data.Sequence as S

main = interact $ f . map digitToInt . head

nIters = 100
n = 9
dec 0 = n - 1
dec x = x - 1

g (x:<|x2:<|x3:<|x4:<|xs) = g' x (dec x) (x2<|x3<|x4<|S.empty) xs
  where
    g' x0 x cs ds = if x `elem` cs then g' x0 (dec x) cs ds else g'' x0 x cs ds
    g'' x0 x cs ds = let (ds1, _:<|ds2) = spanl (/=x) ds in (ds1 >< x<|cs >< ds2) |> x0

f xs = Str $ concatMap (show . (+1)) $ xs2 >< xs1
  where
    xs' = map (+ (-1)) xs
    (xs1, _:<|xs2) = spanl (/=0) $ applyN nIters g $ S.fromList xs'
