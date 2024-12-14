{-# LANGUAGE LambdaCase, TypeSynonymInstances, FlexibleInstances, TupleSections #-}
import Control.Monad.State
import Data.Either
import Data.Set (Set,member,insert,empty)
import Text.ParserCombinators.Parsec (many1,digit,manyTill,sepEndBy,string,char,parse)

(...) = (.) . (.)

type D = [Int]
type S = (D,D)
type R = Either D D

parseInput :: String -> S
parseInput = either (error . show) id . parse inputData ""
  where inputData = do
          string "Player 1:\n"
          p1 <- (read <$> many1 digit <* char '\n') `manyTill` char '\n'
          string "Player 2:\n"
          p2 <- (read <$> many1 digit) `sepEndBy` char '\n'
          pure (p1,p2)

playGame :: GameState s => s -> Int
playGame = either score score . evalState play

score = sum . zipWith (*) [1..] . reverse

class GameState s where
  player1wins :: s -> Bool
  gameEnded :: s -> Maybe R
  p1wins :: s -> s
  p2wins :: s -> s
  seen :: s -> s

  play :: State s R
  play = gets gameEnded >>= \case
    Just w  -> pure w
    Nothing -> do
      modify seen
      gets player1wins >>= \case
        True  -> modify p1wins
        False -> modify p2wins
      play

instance GameState S where
  player1wins (a:_,b:_) = a > b
  gameEnded (p,[]) = Just $ Left p
  gameEnded ([],p) = Just $ Right p
  gameEnded _      = Nothing
  seen             = id

  p1wins (c1:r1,c2:r2) = (r1++[c1,c2],r2)
  p2wins (c1:r1,c2:r2) = (r1,r2++[c2,c1])

instance GameState (Set S,S) where
  player1wins = evalState $ shouldSubgame >>= \case
      True  -> isLeft <$> subgame
      False -> gets $ player1wins . snd
  gameEnded (s,p) = if p `member` s then Just (Left $ fst p) else gameEnded p
  seen (s,p)      = (p `insert` s,p)

  p1wins (s,p) = (s,p1wins p)
  p2wins (s,p) = (s,p2wins p)

shouldSubgame :: State (Set S,S) Bool
shouldSubgame = do
  (p1,p2) <- gets snd
  pure $ head p1 < length p1 && head p2 < length p2

subgame :: State (Set S,S) R
subgame = withState p' play
  where p' (s,(c1:r1,c2:r2)) = (s,(take c1 r1, take c2 r2))

main = getContents >>= solve

solve dat = do
  let p = parseInput dat
  print $ part1 p
  print $ part2 p
  where
    part1 = playGame
    part2 = playGame . (empty::Set S,)
