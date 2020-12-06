import AOC
import qualified Data.Set as S

main = interactg $ sum . map (S.size . S.fromList . concat)
