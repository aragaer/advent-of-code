import Text.ParserCombinators.Parsec

main = getContents >>= solve

calc :: GenParser Char () Int -> GenParser Char () Int
calc rule = sum <$> rule `sepEndBy` newline

expr1 = term expr1 `chainl1` (add <|> mul)
expr2 = term expr2 `chainl1` add `chainl1` mul

term e = (parens e <|> integer) <* optional (char ' ')
add = (char '+' >> return (+) <* space)
mul = (char '*' >> return (*) <* space)
parens = between (char '(') (char ')')
integer = read <$> many1 digit

solve dat = do
  print $ answer expr1 dat
  print $ answer expr2 dat
  where
    answer p = either (error . show) id . parse (calc p) ""
