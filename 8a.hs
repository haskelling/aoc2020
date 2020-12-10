import AOC
import qualified Data.Vector as V
import qualified Data.Set as S

main = interact $ f . ltov . parselist p

data Instruction = Acc | Jmp | Nop deriving (Show, Eq, Read, Ord, Bounded, Enum)

p :: Parser (Instruction, Int)
p = do
  instr <- enump
  space
  sgn <- choice [char '+' >> return 1, char '-' >> return (-1), return 1]
  arg <- read <$> many1 digit
  return (instr, sgn * arg)

f :: Vector (Instruction, Int) -> Int
f prg = exec 0 0 S.empty
  where
    exec a ip s =
      if ip `S.member` s
        then
          a
        else
          let
            s' = S.insert ip s
            ip' = succ ip
          in
            case prg V.!? ip of
              Just (Acc, i) -> exec (a + i) ip' s'
              Just (Jmp, i) -> exec a (ip + i) s'
              Just (Nop, _) -> exec a ip' s'
