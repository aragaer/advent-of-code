import qualified Data.Set as S
import Text.ParserCombinators.Parsec

inputData = do
  string "Player 1:\n"
  p1 <- (read <$> many1 digit <* char '\n') `manyTill` char '\n'
  string "Player 2:\n"
  p2 <- (read <$> many1 digit) `sepEndBy` char '\n'
  pure (p1,p2)

parseInput :: String -> ([Int],[Int])
parseInput = either (error . show) id . parse inputData ""

part1 p1 p2 = score $ game1 p1 p2
part2 p1 p2 = either score score $ game2 S.empty p1 p2

score = sum . zipWith (*) [1..] . reverse

game1 [] s = s
game1 s [] = s
game1 (c1:c1s) (c2:c2s) | c1 < c2 = game1 c1s (c2s++[c2,c1])
                        | c1 > c2 = game1 (c1s++[c1,c2]) c2s
                        | otherwise = error "Same cards"

game2 :: S.Set ([Int],[Int]) -> [Int] -> [Int] -> Either [Int] [Int]
game2 s p [] = Left p
game2 s [] p = Right p
game2 s (c1:c1s) (c2:c2s) = if (c1:c1s,c2:c2s) `S.member` s
  then Left (c1:c1s)
  else case winner of
      Left _  -> game2 s' (c1s++[c1,c2]) c2s
      Right _ -> game2 s' c1s (c2s++[c2,c1])
    where
      winner = if length c1s >= c1 && length c2s >= c2
            then game2 s' (take c1 c1s) (take c2 c2s)
            else if c1 > c2 then Left [] else Right []
      s' = S.insert (c1:c1s,c2:c2s) s

main = getContents >>= solve

solve dat = do
  let (p1,p2) = parseInput dat
  print $ part1 p1 p2
  print $ part2 p1 p2
