import Data.Either
import Data.Map (Map, (!), union, fromList)
import Text.ParserCombinators.Parsec

type Ruleset = Map Int [Rule]
data Rule = Character Char | Sequence [Int] deriving Show

inputData :: GenParser Char st (Ruleset,[String])
inputData = do
  rules <- (rule <* char '\n') `manyTill` char '\n'
  strings <- (many1 letter) `sepEndBy` char '\n'
  pure (fromList rules, strings)

rule = do
  i <- read <$> digit `manyTill` char ':'
  contents <- alternative `sepBy` char '|'
  pure (i,contents)

alternative = optional space >> (charRule <|> sequenceRule) <* optional (char ' ')

charRule = Character <$> between (char '"') (char '"') letter
sequenceRule = Sequence <$> (read <$> many1 digit) `sepEndBy` (char ' ')

validate :: Ruleset -> [String] -> [String]
validate ruleset strings = filter (any (=="") . match ruleset (Sequence [0])) strings

match rset (Character c) "" = fail "no match"
match rset (Character c) (x:xs) | c == x    = pure xs
                                | otherwise = fail "no match"

match rset (Sequence []) s     = pure s
match rset (Sequence (r:rs)) s = do
  r <- rset ! r
  m <- match rset r s
  match rset (Sequence rs) m

main = getContents >>= solve

newrules = either (error . show) fromList
         $ parse (rule `sepEndBy` char '\n') ""
         $ unlines [ "8: 42 | 42 8"
                   , "11: 42 31 | 42 11 31"]

solve dat = do
  let (ruleset,strings) = parseInputData dat
  print $ length $ validate ruleset strings
  print $ length $ validate (newrules `union` ruleset) strings

parseInputData = (either (error . show) id) . parse inputData ""

-- Second attempt, currently incorrect
applyRuleset rs = parse (parserFromRule rs (Sequence [0]) >> eof) ""

parserFromRule rset (Character c) = char c >> pure ()
parserFromRule rset (Sequence s) = mapM_ (fromChoice . (rset !)) s
  where fromChoice = choice . map (try . parserFromRule rset)


solve2 dat = do
  let (ruleset,strings) = parseInputData dat
  print $ length $ rights $ map (applyRuleset ruleset) strings
  print $ length $ rights $ map (applyRuleset (newrules `union` ruleset)) strings

h dat = do
  solve2 dat
  let (r,s) = parseInputData dat
  let n = newrules `union` r
  putStrLn $ unlines $ map (\s -> show (s, applyRuleset n s)) correct

correct = words "bbabbbbaabaabba \
\ babbbbaabbbbbabbbbbbaabaaabaaa \
\ aaabbbbbbaaaabaababaabababbabaaabbababababaaa \
\ bbbbbbbaaaabbbbaaabbabaaa \
\ bbbababbbbaaaaaaaabbababaaababaabab \
\ ababaaaaaabaaab \
\ ababaaaaabbbaba \
\ baabbaaaabbaaaababbaababb \
\ abbbbabbbbaaaababbbbbbaaaababb \
\ aaaaabbaabaaaaababaa \
\ aaaabbaabbaaaaaaabbbabbbaaabbaabaaa \
\ aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
