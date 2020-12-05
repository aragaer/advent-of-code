import Data.List

main = getContents >>= solve

passToId s = f 0 s
  where
    f r "" = r
    f r (x:xs) = f (r*2 + if x `elem` "BR" then 1 else 0) xs

findMissing ids = searchIn $ zip ids (tail ids)
  where
    searchIn [] = error "Nothing left"
    searchIn ((a,b):xs) = if a - b == 2 then a - 1 else searchIn xs

solve dat = do
  let ids = reverse $ sort $ map passToId $ lines dat
  print $ head ids
  print $ findMissing ids
