import Data.List

(...) = (.) . (.)

step = (`mod` 20201227) ... (*)
run = flip iterate 1 . step
transform = (!!) . run
hack = maybe undefined id . (`elemIndex` run 7)

main = getContents >>= solve

solve dat = do
  let [cardPublic,doorPublic] = map read $ lines dat
  let cardLoop = hack cardPublic
  print $ transform doorPublic cardLoop

h = solve . unlines $ map show [5764801,17807724]
