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

to_entries dat = map (map to_entry . words) $ f (tail ld) [] (head ld)
  where
    ld = lines dat
    f [] r l = r ++ [l]
    f ("":x:xs) r l = f xs (r ++ [l]) x
    f (x:xs) r l = f xs r (l ++ " " ++ x)
    to_entry (x:y:z:':':xs) = ([x,y,z], xs)

validate1 entry = all isPresent rules
  where isPresent (f, _) = isJust $ lookup f entry

validate2 entry = all isValid rules
  where isValid (f, v) = (v <$> lookup f entry) == Just True

solve dat = do
  let entries = to_entries dat
  print $ length $ filter validate1 entries
  print $ length $ filter validate2 entries
