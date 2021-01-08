import AOC

main = interact f

f = count '#' . concat . mapnbsC nbs8 g
  where
    g x ns = case x of
      'L' -> if count '#' ns == 0 then '#' else 'L'
      '#' -> if count '#' ns >= 4 then 'L' else '#'
      x -> x
