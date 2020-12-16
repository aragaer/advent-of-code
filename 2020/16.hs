{-# LANGUAGE LambdaCase #-}
import Data.List
import Text.ParserCombinators.Parsec

main = getContents >>= solve

type Rule = (String,[Int])
type Ticket = [Int]

ticket :: GenParser Char st Ticket
ticket = (read <$> many1 digit) `sepEndBy` char ','

rule :: GenParser Char st Rule
rule = do
  field <- many1 (noneOf ":")
  string ": "
  ranges <- range `sepEndBy` string " or "
  pure (field,concatMap (\(s,e)->[s..e]) ranges)

range :: GenParser Char st (Int,Int)
range = do
  s <- read <$> many1 digit
  char '-'
  e <- read <$> many1 digit
  pure (s,e)

toSections = go [] . lines
  where
    go acc [] = [acc]
    go acc ("":xs) = acc : go [] xs
    go acc (x:xs) = go (acc ++ [x]) xs

f = \case
  Left err -> error $ show err
  Right x -> x

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

removeUsed numbers = if length bad == 0 then numbers else removeUsed numbers'
  where
    bad = filter ((>1) . length) numbers
    good = concat $ filter ((==1) . length) numbers
    numbers' = map (\l -> if length l == 1 then l else filter (not . (`elem` good)) l) numbers

solve dat = do
  let [r,m,o] = toSections dat
  let my = f $ parse ticket "" (m !! 1)
  let other = map (f . parse ticket "") $ tail o
  let rules = map (f . parse rule "") r
  print $ part1 rules other
  print $ part2 rules other my
