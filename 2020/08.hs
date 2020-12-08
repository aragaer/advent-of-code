import Data.Set (member,empty,insert)
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

run code = go (0,0) empty
  where go (acc,ip) seen | ip `member` seen  = Left acc
                         | ip >= length code = Right acc
                         | otherwise         = go (execute acc ip (code !! ip)) (ip `insert` seen)

execute acc ip ("nop",_) = (acc,ip+1)
execute acc ip ("acc",val) = (acc+val,ip+1)
execute acc ip ("jmp",val) = (acc,ip+val)

nop (a,i) _ = (a,i+1)
acc (a,i) v = (a+v,i+1)
jmp (a,i) v = (a,i+v)

part1 code = case run code of
  Left l  -> l
  Right _ -> error "Expected to loop"

part2 code = head $ do
  (i, (c,v)) <- filter ((`elem` ["jmp", "nop"]) . fst . snd) $ zip [0..] code
  let c' = if c == "jmp" then "nop" else "jmp"
  let code' = take i code ++ (c',v) : drop (i+1) code
  case run code' of
    Left _  -> fail "infinite loop"
    Right r -> pure r

solve dat = do
  let code = parseCode dat
  print $ part1 code
  print $ part2 code
