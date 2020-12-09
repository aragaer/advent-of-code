import Data.List

main = getContents >>= solve

isValid pre num = any ((`elem` pre) . (num -)) pre

preamble = 25

part1 = uncurry go . splitAt preamble
  where
    go l [] = error "Must have an incorrect number"
    go l (x:xs) | isValid l x = go (tail l ++ [x]) xs
                | otherwise   = x

part2 numbers num = go [] numbers
  where
    go l1 l2 | sum l1 < num = go (l1 ++ [head l2]) (tail l2)
             | sum l1 > num = go (tail l1) l2
             | otherwise    = minimum l1 + maximum l1

solve dat = do
  let numbers = map read $ lines dat :: [Int]
  let p1 = part1 numbers
  print p1
  print $ part2 numbers p1
