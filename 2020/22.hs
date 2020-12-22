import Data.Either
import Data.Function
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

(...) = (.) . (.)

parseInput :: String -> ([Int],[Int])
parseInput = either (error . show) id . parse inputData ""
  where inputData = do
          string "Player 1:\n"
          p1 <- (read <$> many1 digit <* char '\n') `manyTill` char '\n'
          string "Player 2:\n"
          p2 <- (read <$> many1 digit) `sepEndBy` char '\n'
          pure (p1,p2)

part1 = either score score ... game1
part2 = either score score ... game2 S.empty

score = sum . zipWith (*) [1..] . reverse

game1 = check1 $ step game1 winner1
game2 s = (check2 s) (\p1 p2 -> let s' = S.insert (p1,p2) s in step (game2 s') (winner2 s') p1 p2)

check1 _ p [] = Left p
check1 _ [] p = Right p
check1 game p1 p2 = game p1 p2

check2 s game p1 p2 = if (p1,p2) `S.member` s then Left p1 else check1 game p1 p2

step game winFunc = uncurry game ... gameRound winFunc

gameRound player2wins p1 p2 = if player2wins p1 p2
  then (tail p1,tail p2 ++ [head p2,head p1])
  else (tail p1 ++ [head p1,head p2],tail p2)

winner1 = (<) `on` head
winner2 s p1 p2 = (if shouldSubgame p1 p2 then subgame s else winner1) p1 p2
  where shouldSubgame = (&&) `on` \c -> length c > head c
        subgame s = (isRight ... game2 s) `on` \(c:r) -> take c r

main = getContents >>= solve

solve dat = do
  let (p1,p2) = parseInput dat
  print $ part1 p1 p2
  print $ part2 p1 p2
