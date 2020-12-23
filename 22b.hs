import AOC
import qualified Data.Set as S

main = interactg f

turn :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> (Bool, [Int])
turn s (c:cs, d:ds) | length cs >= c && length ds >= d = r
  where
    s' = S.insert (c:cs, d:ds) s
    r = if (c:cs, d:ds) `S.member` s
          then (True, c:cs)
          else
            turn s' $ let (w, _) = turn S.empty (take c cs, take d ds)
                      in  if w then (cs ++ [c, d], ds) else (cs, ds ++ [d, c])

turn s (c:cs, d:ds) = r
  where
    s' = S.insert (c:cs, d:ds) s
    r = if (c:cs, d:ds) `S.member` s
          then (True, c:cs)
          else
            turn s' $ if c > d then (cs ++ [c, d], ds) else (cs, ds ++ [d, c])

turn _ ([], ds) = (False, ds)
turn _ (cs, []) = (True,  cs)

f [_:p1, _:p2] = sum $ zipWith (*) [n,n-1..] cs'
  where
    cs = map read p1
    ds = map read p2
    (_, cs') = turn S.empty (cs, ds)
    n = length cs'
