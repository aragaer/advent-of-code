import Data.Set hiding (map,filter)
import qualified Data.Set as S (map,filter)

data Point = Point {x::Int, y::Int, z::Int, w::Int} deriving (Eq, Ord, Show)

thd (_,_,a) = a

main = getContents >>= solve

readData :: String -> Set Point
readData dat = fromList $ concatMap (\(y,l) -> map (\(x,_) -> Point x y 0 0)
                                             $ filter ((=='#') . snd)
                                             $ zip [0..] l)
                        $ (zip [0..] . lines) dat

step inactive living = living `union` born \\ dead
  where
    dead = S.filter (not . survives) living
    born = S.filter activates $ inactive living
    survives = (`elem` [2,3]) . (+ (-1)) . size . (intersection living) . neighboursOf
    activates = (== 3) . size . (intersection living) . neighboursOf

inactive3 :: Set Point -> Set Point
inactive3 r = fromList [Point x y z 0 | x <- f x, y <- f y, z <- f z]
  where f g = [findMin (S.map g r) - 1..findMax (S.map g r) + 1]

inactive4 :: Set Point -> Set Point
inactive4 r = fromList [Point x y z w | x <- f x, y <- f y, z <- f z, w <- f w]
  where f g = [findMin (S.map g r) - 1..findMax (S.map g r) + 1]

neighboursOf :: Point -> Set Point
neighboursOf p = fromList [Point x y z w | x <- rx, y <- ry, z <- rz, w <- rw]
  where [rx,ry,rz,rw] = map ((\n->[n-1..n+1]) . flip ($) p) [x,y,z,w]

solve dat = do
  let initial = readData dat
  print . size . (!! 6) . iterate (step inactive3) $ initial
  print . size . (!! 6) . iterate (step inactive4) $ initial
