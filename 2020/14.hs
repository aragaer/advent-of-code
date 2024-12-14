{-# LANGUAGE LambdaCase #-}
import qualified Data.Map as Map
import Data.Bits
import Text.ParserCombinators.Parsec

main = getContents >>= solve

data Op = Mask String | Store Int Int deriving Show

part1 = go Map.empty default_mask
  where
    go :: Map.Map Int Int -> (Int, Int) -> [Op] -> Int
    go mem mask (Mask m:xs) = go mem (new_mask m) xs
    go mem mask (Store a v:xs) = go (store mem mask a v) mask xs
    go mem mask [] = Map.foldr (+) 0 mem
    default_mask = (0,0)

new_mask :: String -> (Int, Int)
new_mask = go 0 0
  where
    go z o []       = (z,o)
    go z o ('X':xs) = go (shift z 1) (shift o 1) xs
    go z o ('0':xs) = go (shift z 1 .|. 1) (shift o 1) xs
    go z o ('1':xs) = go (shift z 1) (shift o 1 .|. 1) xs

store mem (z,o) addr dat = Map.insert addr value mem
  where value = dat .&. (complement z) .|. o

parseProg :: GenParser Char st [Op]
parseProg = command `sepEndBy` char '\n'
  where
    command = do
      many1 letter >>= \case
        "mask" -> eq >> Mask <$> many1 alphaNum
        "mem"  -> do
          addr <- read <$> between (char '[') (char ']') (many1 digit)
          eq >> Store addr <$> read <$> many1 digit
    eq = between space space (char '=')

part2 = go Map.empty default_mask
  where
    go mem mask (Mask m:xs) = go mem (new_mask2 m) xs
    go mem mask (Store a v:xs) = go (store2 mem mask a v) mask xs
    go mem mask [] = Map.foldr (+) 0 mem
    default_mask = ([],0)

new_mask2 :: String -> ([Int], Int)
new_mask2 = go [] 0
  where
    go f o []       = (f,o)
    go f o ('X':xs) = go (length xs:f) (shift o 1) xs
    go f o ('0':xs) = go f (shift o 1) xs
    go f o ('1':xs) = go f (shift o 1 .|. 1) xs

store2 mem (f,o) addr value = flip Map.union mem $ Map.fromList $ zip addrs $ repeat value
  where addrs = floating f (addr .|. o)

floating [] a = [a]
floating (b:bs) a = [a `setBit` b, a `clearBit` b] >>= floating bs

solve dat = do
  let commands = case parse parseProg "" dat of
        Left err -> error $ show err
        Right c -> c
  print $ part1 commands
  print $ part2 commands
