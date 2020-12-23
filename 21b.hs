{-# LANGUAGE TupleSections #-}
import AOC
import qualified Data.Map as M

main = interact f

f s = intercalate "," $ map (head . snd) remainingPossibilities
  where
    g [is, as] = (words is, splitOn ", " $ init as)
    mapping = map (g . splitOn " (contains ") s
    allIs = nub $ concatMap fst mapping
    allAs = nub $ concatMap snd mapping
    givenPossibilities = concatMap (\(is, as) -> map (,is) as) mapping
    asToIs = M.fromList $ map (,allIs) allAs
    allPossibilities = foldl' (\m (a, is) -> M.update (\is' -> Just $ is' `intersect` is) a m) asToIs givenPossibilities

    removeKnowns m = let knowns = concat $ filter ((==1) . length) $ map snd m
                     in  map (\(x, ys) -> (x, if length ys /= 1 then ys \\ knowns else ys)) m
    remainingPossibilities = sort $ converge removeKnowns $ M.toList allPossibilities
