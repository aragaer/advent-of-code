import Data.Complex

data MyState = MyState {getPosition:: Complex Float, getWaypoint::Complex Float} deriving (Eq, Show)

main = getContents >>= solve

part1 commands = let npos = foldl move MyState {getPosition=0:+0, getWaypoint=1:+0} commands
                 in round $ (abs . realPart . getPosition) npos + (abs . imagPart . getPosition) npos

part2 commands = let npos = foldl move' MyState {getPosition=0:+0, getWaypoint=10:+1} commands
                 in round $ (abs . realPart . getPosition) npos + (abs . imagPart . getPosition) npos

move :: MyState -> (Char, Float) -> MyState
move s ('N',v) = strafe (0:+v) s
move s ('S',v) = strafe (0:+(-v)) s
move s ('E',v) = strafe (v:+0) s
move s ('W',v) = strafe ((-v):+0) s

move s ('L',v) = case v of
  90 -> rotate (0:+1) s
  180 -> rotate ((-1):+0) s
  270 -> rotate (0:+(-1)) s

move s ('R',v) = case v of
  270 -> rotate (0:+1) s
  180 -> rotate ((-1):+0) s
  90 -> rotate (0:+(-1)) s

move s ('F',v) = s {getPosition=(v:+0)*getWaypoint s + getPosition s}

strafe p s = s {getPosition=p + getPosition s}
rotate d s = s {getWaypoint=d * getWaypoint s}

move' :: MyState -> (Char, Float) -> MyState
move' s ('N',v) = strafe' (0:+v) s
move' s ('S',v) = strafe' (0:+(-v)) s
move' s ('E',v) = strafe' (v:+0) s
move' s ('W',v) = strafe' ((-v):+0) s
move' a b = move a b

strafe' p s = s {getWaypoint=p + getWaypoint s}

parseCommand :: String -> (Char, Float)
parseCommand c = (head c, read $ tail c)

solve dat = do
  let commands = map parseCommand $ lines dat
  print $ part1 commands
  print $ part2 commands
