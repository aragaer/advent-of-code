import System.IO

main = do
  inh <- openFile "input01" ReadMode
  dat <- hGetContents inh
  let numbers = map read (lines dat) :: [Int]
  let (x, y) = head [(x, y) | x <- numbers,
                              y <- numbers, x + y == 2020]
  print $ x * y
  let (x, y, z) = head [(x, y, z) | x <- numbers,
                                    y <- numbers,
                                    z <- numbers, x + y + z== 2020]
  print $ x * y * z
