import Control.Monad (foldM)
import Data.List

main = getContents >>= solve

readIds :: String -> [Int]
readIds = go ""
  where go s [] = [read s]
        go s (',':xs) = (if s == "x" then [] else [read s]) ++ go "" xs
        go s (x:xs) = go (s++[x]) xs

readIds' :: String -> [(Integer,Integer)]
readIds' = toPairs . go ""
  where
    go :: String -> String -> [Maybe Int]
    go s [] = [f s]
    go s (',':xs) = f s : go "" xs
    go s (x:xs) = go (s++[x]) xs
    f :: String -> Maybe Int
    f "x" = Nothing
    f s = Just $ read s
    toPairs :: [Maybe Int] -> [(Integer,Integer)]
    toPairs = p . zip [0..]
    p :: [(Integer,Maybe Int)] -> [(Integer,Integer)]
    p [] = []
    p ((_,Nothing):xs) = p xs
    p ((i,Just v):xs) = (i,toInteger v):p xs

part1 t l = f m * (m `mod` t)
  where m = minimumBy (\a b -> f a `compare` f b) l
        f a = a - t `mod` a

egcd a b = aux a b 1 0 0 1
  where aux r 0  x y _  _  = (r,x,y)
        aux r r' x y x' y' = aux r' r'' x' y' x'' y''
          where r'' = r `rem` r'
                q   = r `div` r'
                x'' = x - q * x'
                y'' = y - q * y'

invert a m = case egcd a m of
              (1,x,_) -> Just x
              _       -> Nothing

chineseRemainder :: [(Integer,Integer)] -> Maybe (Integer,Integer)
chineseRemainder = foldM aux (0,1)
    where aux (a,p) (b,q)
              | (a-b) `rem` k == 0 = Just (x, kmn)
              | otherwise          = Nothing
              where k       = gcd p q
                    m       = p `div` k
                    n       = q `div` k
                    (_,_,β) = k `egcd` (m*n)
                    (_,_,δ) = m `egcd` q
                    (_,_,ζ) = n `egcd` p
                    kmn     = p*n
                    x       = (a*β*m*n+a*δ*k*n+b*ζ*k*m) `rem` kmn

part2 l = case chineseRemainder l of
    Nothing -> error "Solution should exist"
    Just (x,y) -> answer x y
  where
    answer x y | x < 0     = (-x)
               | otherwise = y-x

solve dat = do
  let l = lines dat
  print $ part1 (read (l !! 0)) (readIds (l !! 1))
  print $ part2 $ readIds' (l !! 1)
