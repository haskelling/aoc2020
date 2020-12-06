import AOC
import qualified Data.Set as S

main = interactg $ sum . map (S.size . foldl1' S.intersection . map S.fromList)
