main = getContents >>= solve

tree_at xch ych (x, row) = (x `mod` xch == 0) && (row !! y) == '#'
  where
    y = ych * div x xch `mod` length row

trees_on_slope x y trees = length $ filter (tree_at x y) $ zip [0..] trees

part1 = trees_on_slope 1 3

part2 trees = product slopes
  where
    slopes = [trees_on_slope 1 1 trees
             , trees_on_slope 1 3 trees
             , trees_on_slope 1 5 trees
             , trees_on_slope 1 7 trees
             , trees_on_slope 2 1 trees]

solve dat = do
  let trees = lines dat
  print $ part1 trees
  print $ part2 trees
