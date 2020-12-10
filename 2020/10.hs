import Data.List

main = getContents >>= solve

part1 numbers = count 1 * (1 + count 3)
 where
   s = map (uncurry (-)) $ zip (0:numbers) numbers
   count i = length $ filter (== -i) s

part2 = product . map combinations . groups

groups = go 0 1
  where
    go _ l [] = [l]
    go n l (x:xs) | x == n+3  = l : go x 1 xs
                  | otherwise = go x (l+1) xs

combinations x | x < 1     = 0
               | x == 1    = 1
               | otherwise = combinations (x-1) + combinations (x-2) + combinations (x-3)

solve dat = do
  let numbers = sort $ map read $ lines dat :: [Int]
  print $ part1 numbers
  print $ part2 numbers
