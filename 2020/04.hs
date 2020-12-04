import Data.Char
import Data.Maybe
import Text.Read

main = getContents >>= solve

type Validator = String -> Bool

rules :: [(String, Validator)]
rules = [ ("byr", intInRange 1920 2002)
        , ("iyr", intInRange 2010 2020)
        , ("eyr", intInRange 2020 2030)
        , ("hgt", validHeight)
        , ("hcl", validHair)
        , ("ecl", (`elem` [ "amb", "blu", "brn", "gry"
                          , "grn", "hzl", "oth"]))
        , ("pid", validPid)]

intInRange :: Int -> Int -> Validator
intInRange f t s = case readMaybe s of
  Just x -> f <= x && x <= t
  _      -> False

validHeight s = validHeight' suffix prefix
  where
    (prefix, suffix) = splitAt (length s - 2) s
    validHeight' "cm" = intInRange 150 193
    validHeight' "in" = intInRange 59 76
    validHeight' _    = const False

validHair ('#':xs) = length xs == 6 && all isHexDigit xs
validHair _        = False

validPid s = length s == 9 && all isDigit s

toEntries = go [] . lines
  where
    go acc [] = [toFields acc]
    go acc ("":xs) = toFields acc : go [] xs
    go acc (x:xs) = go (acc ++ ' ' : x) xs
    toFields = map toField . words
    toField (x:y:z:':':xs) = ([x,y,z], xs)

validate rules = ((flip all) rules) . check
check e (f, v) = (v <$> lookup f e) == Just True

solve dat = do
  let entries = toEntries dat
  print $ length $ filter (validate $ map (const True <$) rules) entries
  print $ length $ filter (validate rules) entries
