import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

parseInput :: String -> [[(Int,Int)]]
parseInput = either (error . show) id . parse inputData ""
 where inputData = inputLine `sepEndBy` char '\n'
       inputLine = many1 (ew 2 0 <|> north <|> south)
       ew :: Int -> Int -> GenParser Char st (Int,Int)
       ew i j =   do {char 'e'; pure (i,j)}
              <|> do {char 'w'; pure ((-i),j)}
       north = char 'n' >> ew 1 2
       south = char 's' >> ew 1 (-2)

step :: S.Set (Int,Int) -> S.Set (Int,Int)
step living = living `S.union` born S.\\ dead
  where
    born = S.filter ((== 2) . countLivingNeighbours) adjacent
    dead = S.filter (not . (`elem` [1,2]) . countLivingNeighbours) living
    adjacent = S.unions $ S.map neighbours living
    countLivingNeighbours = S.size . S.intersection living . neighbours

neighbours :: (Int,Int) -> S.Set (Int,Int)
neighbours (i,j) = S.fromList [(i+2,j),(i+1,j+2),(i-1,j+2),(i-2,j),(i-1,j-2),(i+1,j-2)]

initialMap input = M.keysSet . M.filter id $ foldl flipTile M.empty input

part1 = S.size

part2 = go 100
  where go 0 = S.size
        go x = go (x-1) . step

flipTile m k = M.insertWith (/=) (navigate k) True m
navigate = foldl (\(a,b) (c,d) -> (a+c,b+d)) (0,0)

main = getContents >>= solve
h = readFile "input24_" >>= solve

solve dat = do
  let input = parseInput dat
  let initial = initialMap input
  print $ part1 initial
  print $ part2 initial
