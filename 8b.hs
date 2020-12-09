import AOC
import qualified Data.Vector as V
import qualified Data.Set as S

main = interact $ f . ltov . rights . map (parse p)

data Instruction = Acc | Jmp | Nop deriving (Show, Eq, Read, Ord, Bounded, Enum)

p :: Parser (Instruction, Int)
p = do
  instr <- enump
  space
  sgn <- choice [char '+' >> return 1, char '-' >> return (-1), return 1]
  arg <- read <$> many1 digit
  return (instr, sgn * arg)

mapAt i f v = v V.// [(i, f $ v V.! i)]

f prg = head $ do
  i <- [0..]
  Right a <- return $ f' $ mapAt i change prg
  return a
  where
    change (Nop, j) = (Jmp, j)
    change (Jmp, j) = (Nop, j)
    change x = x

f' :: Vector (Instruction, Int) -> Either Int Int
f' prg = exec 0 0 S.empty
  where
    exec a ip s =
      if ip `S.member` s
        then
          Left a
        else
          let
            s' = S.insert ip s
            ip' = succ ip
          in
            case prg V.!? ip of
              Nothing -> Right a
              Just (Acc, i) -> exec (a + i) ip' s'
              Just (Jmp, i) -> exec a (ip + i) s'
              Just (Nop, _) -> exec a ip' s'
