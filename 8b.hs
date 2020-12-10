import AOC
import Control.Monad.State
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

mapAt i f v = v V.// [(i, f $ v V.! i)]

f prg = head $ catMaybes $ do
  i <- [0..]
  return $ f' $ mapAt i change prg
  where
    change (Nop, j) = (Jmp, j)
    change (Jmp, j) = (Nop, j)
    change x = x

f' :: Vector (Instruction, Int) -> Maybe Int
f' prg = case runState exec (0, 0, S.empty) of
           (False, (a, _, _)) -> Just a
           _                  -> Nothing
  where
    exec = do
      (a, ip, s) <- get
      if ip `S.member` s
        then
          return True
        else do
          let
            s' = S.insert ip s
            ip' = succ ip
          case prg V.!? ip of
            Nothing -> return False
            Just op -> do
              case op of
                (Acc, i) -> put (a + i, ip',    s')
                (Jmp, i) -> put (a,     ip + i, s')
                (Nop, _) -> put (a,     ip',    s')
              exec
