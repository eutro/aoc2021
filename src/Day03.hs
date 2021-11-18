import Data.List

collisions :: [[Bool]] -> (Int, Int) -> Int
collisions trees (dx, dy) =
  loop 0 trees 0
  where
    width = length $ head trees :: Int
    loop :: Int -> [[Bool]] -> Int -> Int
    loop n [] x = n
    loop n (line : tail) x =
      loop nn ntail nx
      where
        nn = n + if line !! x then 1 else 0
        ntail = (drop dy (line : tail))
        nx = (x + dx) `rem` width

main :: IO ()
main = do
  ls <- getContents
  let trees = map (map (== '#')) (lines ls) :: [[Bool]]
  putStrLn $ show $ collisions trees (3, 1)
  putStrLn $ show $ product $ map (collisions trees) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
