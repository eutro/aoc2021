import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import Util

main :: IO ()
main = do
  input <- getContents
  let insns' = map (splitOn " ") (lines input)
  let insns = zipWith (curry id) (map head insns') (map ((read :: String -> Int) . head . tail) insns')
  solve advance1 insns
  solve advance2 insns
  where solve foldf insns =
          putStrLn $ show $ uncurry (*) $ fst $ foldl foldf ((0, 0), 0) insns
        advance1 ((h, d), a) (dir, x) =
          case dir of
            "forward" -> ((h + x, d), a)
            "up" -> ((h, d - x), a)
            "down" -> ((h, d + x), a)
        advance2 ((h, d), a) (dir, x) =
          case dir of
            "forward" -> ((h + x, d + a * x), a)
            "up" -> ((h, d), a - x)
            "down" -> ((h, d), a + x)
