main = getContents >>= solve

toGroups = go [] . lines
  where
    go acc [] = [acc]
    go acc ("":xs) = acc:go [] xs
    go acc (x:xs) = go (x:acc) xs

countAnswersAny group = length $ filter (`elem` unwords group) ['a'..'z']
countAnswersAll group = length $ filter (\c -> all (c `elem`) group) ['a'..'z']

solve dat = do
  let groups = toGroups dat
  print $ sum $ map countAnswersAny groups
  print $ sum $ map countAnswersAll groups
