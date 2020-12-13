import AOC
import Data.Bool
import qualified Data.Vector as V

main = interact $ f . ltov . map (ltov . map (bool 0 1 . (=='L')))

step = map8nbs g
  where
    g x ns = case x of
      1 -> if count 2 ns == 0 then 2 else 1
      2 -> if count 2 ns >= 4 then 1 else 2
      x -> x

f m = sum $ map (count 2 . vtol) $ vtol $ converge step m
