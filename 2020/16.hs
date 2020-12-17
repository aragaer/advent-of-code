{-# LANGUAGE LambdaCase #-}
import Data.List
import Text.ParserCombinators.Parsec

main = getContents >>= solve

type Rule = (String,[Int])
type Ticket = [Int]

inputData :: GenParser Char st ([Rule],Ticket,[Ticket])
inputData = do
  rules <- (rule <* char '\n') `manyTill` char '\n'
  string "your ticket:\n"
  my <- ticket
  string "\n\nnearby tickets:\n"
  other <- ticket `sepEndBy` char '\n'
  pure (rules,my,other)

number :: GenParser Char st Int
number = read <$> many1 digit

ticket :: GenParser Char st Ticket
ticket = number `sepBy` char ','

rule :: GenParser Char st Rule
rule = do
  field <- anyChar `manyTill` char ':'
  space
  ranges <- concat <$> range `sepBy` string " or "
  pure (field,ranges)
  where range = do
          s <- number
          char '-'
          e <- number
          pure [s..e]

invalid rules ticket = filter (\t -> not $ any (\(_,r) -> t `elem` r) rules) ticket

part1 rules tickets = sum $ concatMap (invalid rules) tickets
part2 rules tickets my = product $ map (\(_,i) -> my !! head i)
                                 $ filter (\((f,_),_) -> "departure" `isPrefixOf` f)
                                 $ zip rules numbers
  where
    valid = filter ((== 0) . length . invalid rules) tickets
    starting = map (\_ -> [0..length my -1]) rules
    numbers = rulesToNumbers rules starting valid

rulesToNumbers rules numbers [] = removeUsed numbers
rulesToNumbers rules numbers (t:ts) = rulesToNumbers rules numbers' ts
  where numbers' = map (\(r,ns) -> valid r t ns) $ zip rules numbers
        valid (f,v) ticket numbers = filter (\n -> (ticket !! n) `elem` v) numbers

removeUsed numbers = if all isGood numbers then numbers else removeUsed numbers'
  where
    good = concat $ filter isGood numbers
    isGood = (== 1) . length
    numbers' = map (\l -> if isGood l then l else filter (not . (`elem` good)) l) numbers

solve dat = do
  let (rules,my,other) = either (error . show) id $ parse inputData "" dat
  print $ part1 rules other
  print $ part2 rules other my
