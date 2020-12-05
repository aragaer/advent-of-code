import Data.List
import Numeric

main = getContents >>= solve

findMissing ids = searchIn $ zip ids (tail ids)
  where
    searchIn [] = error "Nothing left"
    searchIn ((a,b):xs) = if a - b == 2 then a - 1 else searchIn xs

passToId = fst . head . readInt 2 (`elem` "FBRL") f
  where f = fromEnum . (`elem` "BR")

solve dat = do
  let ids = reverse $ sort $ map passToId $ lines dat
  print $ head ids
  print $ findMissing ids
