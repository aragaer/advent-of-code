{-# LANGUAGE LambdaCase #-}
import Text.ParserCombinators.Parsec
import System.IO

input :: GenParser Char st [(Int,Int,Char,String)]
input = do
  result <- many rule
  eof
  return result

rule = do
  min <- read <$> many1 digit
  char '-'
  max <- read <$> many1 digit
  space
  c <- letter
  char ':'
  space
  pass <- many1 (noneOf "\n")
  char '\n'
  return (min,max,c,pass)


check1 min max _ "" = min <= 0 && max >= 0
check1 min max c l | c == head l = check1 (min-1) (max-1) c (tail l)
                   | otherwise   = check1 min max c (tail l)

xor True a = not a
xor False a = a

check2 p1 p2 c pass = xor (c == pass !! (p1-1)) (c == pass !! (p2-1))

unc4 f (a,b,c,d) = f a b c d

solve dat = do
  case parse input "test" dat of
    Left error -> print "parse error"
    Right entries -> do
      print $ length $ filter (unc4 check1) entries
      print $ length $ filter (unc4 check2) entries

main = getContents >>= solve
