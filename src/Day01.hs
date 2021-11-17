-- 2020 day 1
import Data.List
import qualified Data.Set as Set
import Data.Maybe (fromJust)

main :: IO ()
main = do contents <- getContents
          let ints = map read (lines contents) :: [Integer]
          let set = Set.fromList ints
          putStrLn $ show $ head $ do
            x <- ints
            let y = 2020 - x
            if Set.member y set
              then [x * y]
              else []
          putStrLn $ show $ head $ do
            x <- ints
            y <- ints
            let z = 2020 - x - y
            if Set.member z set
              then [x * y * z]
              else []
