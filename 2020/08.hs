import Data.Set (member,empty,insert)
import Text.ParserCombinators.Parsec

main = getContents >>= solve

data Instruction = Nop Int | Jmp Int | Acc Int deriving (Eq, Show)

parseCode = map parseLine . lines
  where
    parseLine line = case parse instruction "" line of
     Left err -> error $ "Error in line [" ++ line ++ "]:\n" ++ show err
     Right i -> i
    instruction = do
      ins <- many1 letter
      space
      sign <- char '-' <|> char '+'
      num <- read <$> many1 digit
      let val = if sign == '-' then (- num) else num
      pure $ case ins of
        "nop" -> Nop val
        "jmp" -> Jmp val
        "acc" -> Acc val

run code = go (0,0) empty
  where
    go (acc,ip) seen | ip `member` seen  = Left acc
                     | ip >= length code = Right acc
                     | otherwise         = go ((compCode !! ip) (acc,ip)) (ip `insert` seen)
    compCode = map toIns code
    toIns (Nop v) = nop v
    toIns (Jmp v) = jmp v
    toIns (Acc v) = acc v

nop _ (a,i) = (a,i+1)
acc v (a,i) = (a+v,i+1)
jmp v (a,i) = (a,i+v)

part1 code = case run code of
  Left l  -> l
  Right _ -> error "Expected to loop"

part2 code = head $ do
  (i, c) <- zip [0..] code
  c' <- changeInstruction c
  let code' = take i code ++ c' : drop (i+1) code
  case run code' of
    Left _  -> fail "infinite loop"
    Right r -> pure r
  where
    changeInstruction (Nop v) = pure (Jmp v)
    changeInstruction (Jmp v) = pure (Nop v)
    changeInstruction (Acc _) = fail "do not change acc"

solve dat = do
  let code = parseCode dat
  print $ part1 code
  print $ part2 code
