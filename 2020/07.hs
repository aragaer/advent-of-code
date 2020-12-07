import Data.List (find)
import Data.Maybe
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

type Color = String
data Rule = Rule {getColor::Color, getBags::[(Integer, Color)]} deriving Show

main = getContents >>= solve

rule = do
  color <- color
  string " bags contain "
  contents <- choice [ string "no other bags" >> pure []
                     , bag `sepBy` string ", "]
  char '.'
  pure $ Rule color contents

color :: GenParser Char st Color
color = do
  word1 <- many1 letter
  space
  word2 <- many1 letter
  pure $ word1 ++ ' ':word2

bag :: GenParser Char st (Integer, Color)
bag = do
  number <- read <$> many1 digit
  space
  color <- color
  space
  string $ if number == 1 then "bag" else "bags"
  pure (number, color)

parseRule :: String -> Rule
parseRule line = case parse rule "" line of
    Left err -> error $ "In line " ++ line ++ ":\n" ++ show err
    Right res -> res

searchUp :: [Rule] -> Color -> [Color]
searchUp rules color = do
  thisColor <- getColor <$> filter (any (== color) . map snd . getBags) rules
  thisColor : searchUp rules thisColor

searchDown :: [Rule] -> Color -> Integer
searchDown rules color = sum $ do
  (count, innerColor) <- getBags . fromJust $ find ((== color) . getColor) rules
  pure $ count * (1 + searchDown rules innerColor)

part1 = S.size . S.fromList . (flip searchUp "shiny gold")
part2 = flip searchDown "shiny gold"

solve dat = do
  let rules = map parseRule $ lines dat
  print $ part1 rules
  print $ part2 rules
