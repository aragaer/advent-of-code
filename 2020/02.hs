{-# LANGUAGE LambdaCase #-}
import Text.ParserCombinators.Parsec
import System.IO

input :: GenParser Char st [(Int,Int,Char,String)]
input = rule `sepEndBy` (char '\n')

rule = do
  min <- read <$> many1 digit
  char '-'
  max <- read <$> many1 digit
  space
  c <- letter
  char ':'
  space
  pass <- many1 (noneOf "\n")
  return (min,max,c,pass)

check1 min max c pass = min <= cnt && max >= cnt
  where cnt = length $ filter (== c) pass

check2 p1 p2 c pass = (c == pass !! (p1-1)) /= (c == pass !! (p2-1))

unc4 f (a,b,c,d) = f a b c d

solve dat = do
  case parse input "test" dat of
    Left error -> print "parse error"
    Right entries -> do
      print $ length $ filter (unc4 check1) entries
      print $ length $ filter (unc4 check2) entries

main = getContents >>= solve
