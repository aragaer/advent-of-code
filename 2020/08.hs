import qualified Data.Set as Set
import Text.ParserCombinators.Parsec

main = getContents >>= solve

parseCode = map parseLine . lines
  where parseLine line = case parse instruction "" line of
          Left err -> error $ "Error in line [" ++ line ++ "]:\n" ++ show err
          Right i -> i
        instruction = do
          ins <- many1 letter
          space
          sign <- char '-' <|> char '+'
          val <- read <$> many1 digit
          pure (ins, if sign == '-' then (- val) else val)

run code = go 0 0 Set.empty
  where
    go acc ip seen | ip `Set.member` seen = Left acc
                   | ip >= length code    = Right acc
                   | otherwise            = go nacc nip (ip `Set.insert` seen)
      where
        (nacc, nip) = execute acc ip (code !! ip)

execute acc ip ("nop",_) = (acc,ip+1)
execute acc ip ("acc",val) = (acc+val,ip+1)
execute acc ip ("jmp",val) = (acc,ip+val)

part1 code = case run code of
  Left l -> l
  Right _ -> error "Expected to loop"

part2 code = head $ do
  (i, (c,v)) <- filter ((`elem` ["jmp", "nop"]) . fst . snd) $ zip [0..] code
  let c' = if c == "jmp" then "nop" else "jmp"
  let code' = take i code ++ (c',v) : drop (i+1) code
  case run code' of
    Left a -> []
    Right r -> [r]

solve dat = do
  let code = parseCode dat
  print $ part1 code
  print $ part2 code
