import Data.Array

main = getContents >>= solve

data Spot = Floor | Occupied | Free deriving (Eq, Show)

processSeatsMap :: String -> Array (Int, Int) Spot
processSeatsMap dat = listArray ((0,0), (rows+1, cols+1)) spots
  where
    cols = length (ls !! 0)
    rows = length ls
    spots = concatMap (map f) $ empty : (map (\l -> 'L':l++['L']) ls) ++ [empty]
    ls = lines dat
    empty = take (cols+2) $ repeat 'L'
    f '.' = Floor
    f 'L' = Free
    f '#' = Occupied

getNeighbours spots (i,j) = length $ filter ((== Occupied) . (spots !)) [(x,y) | x <- [i-1..i+1], y <- [j-1..j+1]]

getNeighbours2 spots p = length $ do
  d <- [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
  case getSeatInDirection spots p d of
    Occupied -> pure ()
    Free -> []

getSeatInDirection spots (x,y) (dx,dy) = let
  (nx,ny) = (x+dx,y+dy)
  res = spots ! (nx,ny)
 in case res of
  Floor -> getSeatInDirection spots (nx,ny) (dx,dy)
  otherwise -> res

getChanges neighFunc spots = do
  let ((a,b),(c,d)) = bounds spots
  x <- [a+1..c-1]
  y <- [b+1..d-1]
  let n = neighFunc spots (x,y)
  case spots ! (x,y) of
    Floor -> []
    Occupied -> if n > 4 then pure ((x,y), Free) else []
    Free -> if n == 0 then pure ((x,y), Occupied) else []

part neighFunc seats = let changes = getChanges neighFunc seats
  in if length changes == 0
    then length $ filter (== Occupied) $ elems seats
    else part neighFunc $ seats // changes

-- part1 and part2 have different values for when a seat should be freed
-- but getNeighbours also counts the seat itself while getNeighbours2 doesn't
-- this accidentally makes the numbers equal
part1 = part getNeighbours
part2 = part getNeighbours2

solve dat = do
  let seats = processSeatsMap dat
  print $ part1 seats
  print $ part2 seats
