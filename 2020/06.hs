import qualified Data.Set as Set

main = getContents >>= solve

toGroups = go [] . lines
  where
    go acc [] = [acc]
    go acc ("":xs) = acc:go [] xs
    go acc (x:xs) = go (Set.fromList x:acc) xs

countAnswersAny = Set.size . foldl1 Set.union
countAnswersAll = Set.size . foldl1 Set.intersection

solve dat = do
  let groups = toGroups dat
  print $ sum $ map countAnswersAny groups
  print $ sum $ map countAnswersAll groups
