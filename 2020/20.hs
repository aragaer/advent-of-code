import Data.List
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

rawDragon = [ "                  # "
            , "#    ##    ##    ###"
            , " #  #  #  #  #  #   "]

dragon = M.keys . M.filter (=='#') $ pixelsToMap rawDragon

matchDragon :: M.Map (Int,Int) Char -> Int -> Int -> Bool
matchDragon m ox oy = all (\(x,y) -> (x+ox,y+oy) `M.lookup` m == Just '#') dragon

data Tile = Tile {getId::Int,getImage::[[Char]],getBorders::[Int]} deriving (Eq, Show)

inputData :: GenParser Char st [Tile]
inputData = do
  tile `sepEndBy` char '\n'
  where tile = do
          string "Tile "
          num <- read <$> many1 digit
          string ":\n"
          image <- (many1 $ oneOf ".#") `sepEndBy` char '\n'
          pure $ makeTile num image

{-
Rotations:
0 to 3 - CW (0,90,180,270)
4 - transpose

Border numbers:
0 top
1 "reversed" top
2 right
3 "reversed" right
4 "reversed" bottom
5 bottom
6 "reversed" left
7 left
-}

makeTile num image = Tile num image $ borders image
  where
    borders img = map (borderCode . ($ img)) [ head
                                             , reverse . head
                                             , map last
                                             , reverse . map last
                                             , reverse . last
                                             , last
                                             , reverse . map head
                                             , map head]
    borderCode = foldl (\x c -> x * 2 + fromEnum (c == '#')) 0

fitsLeft = fits 2 7
fitsUp = fits 5 0

fits :: Int -> Int -> Maybe (Tile,Int) -> Tile -> Int -> Bool
fits _ _ Nothing _ _ = True
fits s1 s2 (Just (t1,r1)) t2 r2 = b1 == b2
  where
    b1 = getBorderCode t1 r1 s1
    b2 = getBorderCode t2 r2 s2
    getBorderCode tile rot side = getBorders tile !! (idx `mod` 8)
      where idx = if rot < 4 then side - rot * 2 else rot * 2 - side - 1

rotatePixels :: [[Char]] -> Int -> [[Char]]
rotatePixels p 0 = p
rotatePixels p 1 = transpose $ reverse p
rotatePixels p 2 = reverse $ map reverse p
rotatePixels p 3 = reverse $ transpose p
rotatePixels p r = rotatePixels (transpose p) (r-4)

assemble :: [Tile] -> Int -> M.Map (Int,Int) (Tile,Int)
assemble tiles = head . findRotations tiles

findRotations :: [Tile] -> Int -> [M.Map (Int,Int) (Tile,Int)]
findRotations tiles size = go M.empty tiles
  where
    go :: M.Map (Int,Int) (Tile,Int) -> [Tile] -> [M.Map (Int,Int) (Tile,Int)]
    go r [] = pure r
    go r t = do
      let c = size * size - length t
      let x = c `mod` size
      let y = c `div` size
      let l = getTileRot r (x-1) y
      let u = getTileRot r x (y-1)
      tile <- t
      rotation <- [0..7]
      if fitsLeft l tile rotation && fitsUp u tile rotation
        then go (M.insert (x,y) (tile,rotation) r) (delete tile t)
        else fail "doesn't fit"
    getTileRot :: M.Map (Int,Int) (Tile,Int) -> Int -> Int -> Maybe (Tile,Int)
    getTileRot r x y = M.lookup (x,y) r

paste :: M.Map (Int,Int) (Tile,Int) -> Int -> [[Char]]
paste tiles size = concatMap collectLines [0..size-1]
  where
    collectLines y = map concat . transpose $ map (\x -> pieces M.! (x,y)) [0..size-1]
    pieces = M.map (cut . uncurry rotate) tiles
    cut = map (init . tail) . init . tail
    rotate = rotatePixels . getImage

pixelsToMap :: [[Char]] -> M.Map (Int,Int) Char
pixelsToMap = M.fromList . concatMap (\(y,l) -> map (\(x,c) -> ((x,y),c)) $ zip [0..] l) . zip [0..]

countDragons :: M.Map (Int,Int) Char -> Int
countDragons m = length $ filter (uncurry $ matchDragon m) $ M.keys m

findDragons pixels = sum $ map (countDragons . pixelsToMap . rotatePixels pixels) [0..7]

part1 image size = product $ map (getId . fst) corners
  where corners = map (image M.!) [ (0,0)
                                  , (0,size-1)
                                  , (size-1,0)
                                  , (size-1,size-1)]

part2 pixels = total - findDragons pixels * length dragon
  where total = length $ filter (=='#') $ concat pixels

main = getContents >>= solve

solve dat = do
  let tiles = either (error . show) id $ parse inputData "" dat
  let size = floor . sqrt . fromIntegral $ length tiles
  let image = assemble tiles size
  print $ part1 image size
  let assembled = paste image size
  print $ part2 assembled
