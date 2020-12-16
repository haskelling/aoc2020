import AOC

main = interactg f

attribp :: String -> Int -> Bool
attribp s = or . mapM inRange rules
  where
    [_, rs] = splitOn ": " s
    rules = map (map read . splitOn "-") $ splitOn " or " rs
    inRange [low, high] x = low <= x && x <= high

attribp' :: String -> [Int]
attribp' s = concatMap valid rules
  where
    [_, rs] = splitOn ": " s
    rules = map (map read . splitOn "-") $ splitOn " or " rs
    valid [low, high] = [low..high]

ticketp :: String -> [Int]
ticketp = map read . splitOn ","

f [as, [_, t], _:ts] = sum $ filter matchesNoRules $ concat tickets
  where
    (attribs, tickets) = (map attribp as, map ticketp ts)
    matchesNoRules = not . or . sequence attribs

f' [as, [_, t], _:ts] = sum $ filter (`notElem` validNums) $ concat tickets
  where
    (attribs, _, tickets) = (map attribp' as, ticketp t, map ticketp ts)
    validNums = nub $ concat attribs
