{-# LANGUAGE LambdaCase #-}
import qualified Data.Map.Strict as M
import Text.ParserCombinators.Parsec

main = getContents >>= solve

numbers :: GenParser Char st [Int]
numbers = (read <$> many1 digit) `sepEndBy` char ','

part1 nums turn = game turn (length nums+1) (last nums) (M.fromList $ zip (init nums) [1..])

game turn i n m = if i == turn then n' else n' `seq` game turn (i+1) n' m'
  where
    n' = i - 1 - M.findWithDefault (i-1) n m
    m' = M.insert n (i-1) m

solve dat = do
  let nums = case parse numbers "" dat of
        Left err -> error $ show err
        Right n  -> n
  print $ part1 nums 2020
  print $ part1 nums 30000000
