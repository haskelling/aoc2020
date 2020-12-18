import AOC
import Text.Parsec.Expr

expr = buildExpressionParser table term
term = paren <|> integer
paren = char '(' *> expr <* char ')'
table = [[Infix (char '+' >> return (+)) AssocLeft], [Infix (char '*' >> return (*)) AssocLeft]]

main = interact' $ sum . parselist expr . lines . filter (/=' ')
