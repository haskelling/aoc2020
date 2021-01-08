import AOC
import qualified Data.Set as S

main = interactg f

turn :: S.Set ([Int], [Int]) -> ([Int], Int, [Int], Int) -> (Bool, [Int])
turn !s (c:cs, cl, d:ds, dl) = if h `S.member` s
                                 then (True, c:cs)
                                 else turn s' $ if winner then (cs ++ [c, d], cl + 1, ds, dl - 1) else (cs, cl - 1, ds ++ [d, c], dl + 1)
  where
    h = (c:cs, d:ds)
    s' = S.insert h s
    winner = if cl > c && dl > d
               then fst $ turn S.empty (take c cs, c, take d ds, d)
               else c > d
turn _ ([], _, ds, _) = (False, ds)
turn _ (cs, _, [], _) = (True,  cs)

f [_:p1, _:p2] = sum $ zipWith (*) [n,n-1..] cs'
  where
    cs = map read p1
    ds = map read p2
    (_, cs') = turn S.empty (cs, length cs, ds, length ds)
    n = length cs'
