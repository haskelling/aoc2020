import AOC
import qualified Data.Map as M

main = interact f

f s = sum $ map (\a -> count a $ concatMap fst mapping) $ allIs \\ noAll
  where
    g [is, as] = (words is, splitOn ", " $ init as)
    mapping = map (g . splitOn " (contains ") s
    allIs = nub $ concatMap fst mapping
    allAs = nub $ concatMap snd mapping
    givenPossibilities = concatMap (\(is, as) -> map (,is) as) mapping
    asToIs = M.fromList $ map (,allIs) allAs
    allPossibilities = foldl' (\m (a, is) -> M.update (\is' -> Just $ is' `intersect` is) a m) asToIs givenPossibilities
    noAll = nub $ concat $ M.elems allPossibilities
