import AOC

main = interactg f

attribp :: String -> (String, Int -> Bool)
attribp s = (name, or . mapM inRange rules)
  where
    [name, rs] = splitOn ": " s
    rules = map (map read . splitOn "-") $ splitOn " or " rs
    inRange [low, high] x = low <= x && x <= high

ticketp :: String -> [Int]
ticketp = map read . splitOn ","

f [as, [_, t], _:ts] = product $ map fst $ filter (isPrefixOf "departure" . snd) $ zip ticket fieldNames
  where
    (attribs, ticket, tickets) = (map attribp as, ticketp t, map ticketp ts)
    matchesAnyRule = or . mapM snd attribs
    tickets' = filter (all matchesAnyRule) tickets

    attributes = map filterAttribs $ transpose tickets'
    filterAttribs xs = map fst $ filter (\(_, r) -> all r xs) attribs

    fieldNames = map head $ converge removeKnowns attributes
    removeKnowns names = let knowns = concat $ filter ((==1) . length) names
                             doRemove ns = if length ns /= 1
                                             then filter (`notElem` knowns) ns
                                             else ns
                         in  map doRemove names
