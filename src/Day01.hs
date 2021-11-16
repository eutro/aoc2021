-- 2020 day 1
import Data.List

main :: IO ()
main = do contents <- getContents
          let ints = map read (lines contents) :: [Integer] in do
            putStrLn (case find
                           (\(x, y) -> (x + y == 2020))
                           [(x, y) | x <- ints, y <- ints] of
                        Just (x, y) -> show (x * y)
                        Nothing -> "none")
            putStrLn (case find
                           (\(x, y, z) -> (x + y + z == 2020))
                           [(x, y, z) | x <- ints, y <- ints, z <- ints] of
                        Just (x, y, z) -> show (x * y * z)
                        Nothing -> "none")
            
