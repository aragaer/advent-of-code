import Data.Char
import Data.List hiding (find)
import Data.Vector.Primitive.Mutable hiding (length,take,tail)
import qualified Data.Vector.Primitive.Mutable as V

main = getContents >>= solve
h = solve "389125467"

{-
Emulate single-linked circular list using array
0th cell contains index of the current cup
ith cell contains index of the cup after the cup with label i
-}

type MyEmulatedList = IOVector Int

m'head = flip V.read 0

melFromList :: [Int] -> IO MyEmulatedList
melFromList l = do
  m <- unsafeNew $ length l + 1
  go m 0 l
  where go m i [] = do
          s <- m'head m
          V.write m i s
          pure m
        go m i (x:xs) = V.write m i x >> go m x xs

melToList :: MyEmulatedList -> IO [Int]
melToList m = do
  s <- m'head m
  (s:) <$> go s m s
  where go x m s = do
          p <- V.read m x
          if p == s
            then pure []
            else (p:) <$> go p m s

gameMove :: MyEmulatedList -> IO ()
gameMove m = do
  h <- V.read m 0
  t1 <- V.read m h
  t2 <- V.read m t1
  t3 <- V.read m t2
  a <- V.read m t3
  let h' = nontaken h [t1,t2,t3] (V.length m-1)
  b <- V.read m h'

  -- h->(t1->t2->t3)->a => h->a
  V.write m h a

  -- h'->b => h'->t1->t2->t3->b
  V.write m h' t1
  V.write m t3 b

  -- move current from h to a
  V.write m 0 a

nontaken x t m = let y = if x == 1 then m else x-1
                 in if y `elem` t then nontaken y t m else y

runGame :: Int -> MyEmulatedList -> IO ()
runGame 0 m = V.write m 0 1
runGame x m = gameMove m >> runGame (x-1) m

extend l = l ++ [maximum l+1..1000000]

part1 l = do
  v <- melFromList l
  runGame 100 v
  l <- melToList v
  pure . concat . map show $ tail l

part2 l = do
  v <- melFromList $ extend l
  runGame 10000000 v
  l <- melToList v
  pure . product . take 2 $ tail l

solve dat = do
  let numbers = (map digitToInt $ filter isDigit dat) :: [Int]
  putStrLn =<< part1 numbers
  print =<< part2 numbers
