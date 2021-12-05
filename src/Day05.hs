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
  let iterOf [[x1, y1], [x2, y2]] =
        if x1 == x2
        then map ((,) x1) [min y1 y2 .. max y1 y2]
        else if y1 == y2
        then map (flip (,) y1) [min x1 x2 .. max x1 x2]
        else zip (if x1 > x2 then reverse [x2..x1] else [x1..x2]) (if y1 > y2 then reverse [y2..y1] else [y1..y2])
  let addToMap m point = if point `Map.member` m then Map.adjust (+ 1) point m else Map.insert point 1 m
  let addLine pointToCovered line = foldl addToMap pointToCovered $ iterOf line
  let runOn lines = print $ Map.size $ Map.filter (>= 2) $ foldl addLine Map.empty lines
  let horizOrV [[x1, y1], [x2, y2]] = x1 == x2 || y1 == y2
  runOn $ filter horizOrV ls
  runOn ls
