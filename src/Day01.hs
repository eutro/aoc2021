main :: IO ()
main = do
  input <- getContents
  let lst = map read $ lines input :: [Int]
  putStrLn $ show $ length $ filter id $ zipWith (<) lst (tail lst)
  let window = zipWith3 (\x y z -> x + y + z) lst (tail lst) (tail $ tail lst)
  putStrLn $ show $ length $ filter id $ zipWith (<) window (tail window)
