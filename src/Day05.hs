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
  let ls = map (map (map (read :: String -> Int) . splitOn ",") . splitOn " -> ") $ lines input :: [[[Int]]]
  let range a b =
        if a == b
        then repeat a
        else if a > b
        then reverse [b..a]
        else [a..b]
  let iterOf [[x1, y1], [x2, y2]] = zip (range x1 x2) (range y1 y2)
  let addToMap m point = Map.insertWith (+) point 1 m
  let addLine pointToCovered line = foldl addToMap pointToCovered $ iterOf line
  let runOn lines = print $ Map.size $ Map.filter (>= 2) $ foldl addLine Map.empty lines
  let horizOrV [[x1, y1], [x2, y2]] = x1 == x2 || y1 == y2
  runOn $ filter horizOrV ls
  runOn ls
