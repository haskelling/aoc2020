import AOC
import qualified Data.IntMap as M
import Data.Bits

main = interact $ f . parselist (try loadp <|> maskp)

data Instr = Mask (Int, Int) | Load Int Int deriving (Show, Eq, Read, Ord)

loadp :: Parser Instr
loadp = do
  string "mem["
  addr <- integer
  string "] = "
  val <- integer
  return $ Load addr val

maskp :: Parser Instr
maskp = do
  string "mask = "
  mask <- many1 anyChar
  return $ Mask $ readMask mask

readMask :: String -> (Int, Int)
readMask = foldl' (\x y -> 2 *$ x + readMask' y) 0
  where
    readMask' 'X' = (1, 0)
    readMask' '0' = (0, 0)
    readMask' '1' = (0, 1)

f is = sum $ snd $ foldl' exec (0, M.empty) is
  where
    exec (mask, mem) (Load addr val) = (mask, updateMap addr mask val mem)
    exec (mask, mem) (Mask x) = (x, mem)
    updateMap addr mask val mem = insertMany val (flAddrs mask addr) mem
    insertMany val addrs m = foldl' (\m' a -> M.insert a val m') m addrs
    flAddrs (mask, set) addr = map (\x -> sum x .|. set .|. addr .&. complement mask) $ floatingAddresses mask
    floatingAddresses mask = subsequences [bit b | b <- [0..35], testBit mask b]
