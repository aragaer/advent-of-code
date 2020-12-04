{-# LANGUAGE OverloadedStrings #-}
import Data.Either
import Data.Maybe
import Text.ParserCombinators.Parsec

linesBy :: (a -> Bool) -> [a] -> [[a]]
linesBy pred lst = linesByInt lst [] []
  where
    linesByInt [] res cur = reverse ((reverse cur):res)
    linesByInt (x:xs) res cur | pred x    = linesByInt xs ((reverse cur):res) []
                              | otherwise = linesByInt xs res (x:cur)

lines2pass [] = ""
lines2pass (x:xs) = j x xs
  where
    j r [] = r
    j r (x:xs) = j (r ++ " " ++ x) xs

main = getContents >>= solve

getField [] _ = Nothing
getField (x:xs) f = if (take 3 x) == f then Just (drop 4 x) else getField xs f

data Passport = Passport String String String String String String String

validate1 pass = do
  byr <- getField pass "byr"
  iyr <- getField pass "iyr"
  eyr <- getField pass "eyr"
  hgt <- getField pass "hgt"
  hcl <- getField pass "hcl"
  ecl <- getField pass "ecl"
  pid <- getField pass "pid"
  pure $ Passport byr iyr eyr hgt hcl ecl pid

year :: GenParser Char st Int
year = read <$> (count 4 digit <* eof)

height :: GenParser Char st (Either Int Int)
height = do
  value <- read <$> many1 digit
  unit <- string "cm" <|> string "in"
  eof
  return $ if unit == "cm" then Left value else Right value

hairColor = char '#' >> count 6 hexDigit <* eof

validate2 (Passport byr iyr eyr hgt hcl ecl pid) = do
  byr <- parse year "" byr
  iyr <- parse year "" iyr
  eyr <- parse year "" eyr
  hgt <- parse height "" hgt
  hcl <- parse hairColor "" hcl
  ecl <- parse (count 3 letter <* eof) "" ecl
  pid <- parse (count 9 digit <* eof) "" pid
  pure (byr, iyr, eyr, hgt, ecl, pid)

validate3 (byr, iyr, eyr, hgt, ecl, pid) = do
  cnd (byr < 1920) "Too old"
  cnd (byr > 2002) "Too young"
  cnd (iyr < 2010) "Passport old"
  cnd (iyr > 2020) "Passport not issued yet"
  cnd (eyr < 2020) "Passport expired"
  cnd (eyr > 2030) "Passport will not expire"
  cnd (checkHeight hgt) "Height not ok"
  cnd (not (ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])) "Incorrect eye color"
  pure pid
  where
    cnd x msg = if x then Left msg else Right ()
    checkHeight (Left val) = val < 150 || val > 193
    checkHeight (Right val) = val < 59 || val > 76

solve dat = do
  let entries = map (linesBy (==' ') . lines2pass) $ linesBy (=="") $ lines dat
  let passports = catMaybes $ map validate1 entries
  print $ length passports
  let parsed = map validate2 passports
  let valid = map validate3 $ rights parsed
  print $ length $ rights valid
