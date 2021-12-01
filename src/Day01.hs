main :: IO ()
main = do
  input <- getContents
  let lst = map read $ lines input :: [Int]
  countInc lst
  let window = zipWith3 (\x y z -> x + y + z) lst (tail lst) (drop 2 lst)
  countInc window
  where countInc l = putStrLn $ show $ length $ filter id $ zipWith (<) l (tail l)
