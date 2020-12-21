import Data.List (sort,intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

inputData :: GenParser Char st [(S.Set String,S.Set String)]
inputData = record `sepEndBy` char '\n'
  where
    record = do
      ingredients <- many1 letter `endBy` space
      allergens <- between (string "(contains ") (char ')') (many1 letter `sepBy` string ", ")
      pure (S.fromList ingredients, S.fromList allergens)

inputToMap :: [(S.Set String,S.Set String)] -> M.Map String (S.Set String)
inputToMap = go M.empty
  where
    go r [] = r
    go m ((i,a):xs) = go (M.unionWith S.intersection m $ M.fromSet (const i) a) xs

removeSingles :: M.Map String (S.Set String) -> M.Map String String
removeSingles = M.map (S.elemAt 0) . go
  where
    go m = if null m then M.empty else singles `M.union` go cleared
      where
        singles = M.filter ((==1) . S.size) m
        singleValues = S.fromList . map (S.elemAt 0) $ M.elems singles
        cleared = M.map (S.\\ singleValues) $ m `M.difference` singles

part1 good = sum . map (S.size . (S.intersection good) . fst)
part2 = intercalate "," . map snd . sort . M.toList

main = getContents >>= solve

solve dat = do
  let input = either (error . show) id $ parse inputData "" dat
  let mapped = inputToMap input
  let ingredients = S.unions $ map fst input
  let good = M.foldl S.difference ingredients mapped
  print $ part1 good input
  putStrLn $ part2 $ removeSingles mapped
