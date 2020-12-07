import Data.List (find)
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

type Color = String
data Rule = Rule {getColor::Color, getBags::[(Integer, Color)]} deriving Show

main = getContents >>= solve

rule = do
  color <- color
  string " bags contain "
  contents <- choice [ string "no other bags" >> pure []
                     , (bag `sepBy` string ", ")]
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
  matchingRule <- filter (any (== color) . map snd . getBags) rules
  let thisColor = getColor matchingRule
  thisColor : searchUp rules thisColor

searchDown :: [Rule] -> Color -> [Integer]
searchDown rules color = do
  let rule = case find ((== color) . getColor) rules of
        Nothing -> error $ "Failed to find a rule for " ++ color
        Just r -> r
  (count, innerColor) <- getBags rule
  pure $ count * (1 + (sum $ searchDown rules innerColor))

part1 rules = S.size . S.fromList $ searchUp rules "shiny gold"
part2 rules = sum $ searchDown rules "shiny gold"

solve dat = do
  let rules = map parseRule $ lines dat
  print $ part1 rules
  print $ part2 rules
