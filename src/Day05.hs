import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

main :: IO ()
main = do
  input <- getContents
  let ls = map (read
                . ("(("++) . (++"))")  -- "((x1,x2),(y1,y2))"
                . intercalate "),(" -- "x1,x2),(y1,y2"
                . map (intercalate ",") -- ["x1,x2", "y1,y2"]
                . transpose -- [["x1", "x2"], ["y1", "y2"]]
                . map (splitOn ",") -- [["x1", "y1"], ["x2", "y2"]]
                . splitOn " -> ") -- ["x1,y1", "x2,y2"]
           $ lines input :: [((Int, Int), (Int, Int))]
  let range' (a, b) =
        (case a `compare` b of
           EQ -> repeat . fst
           GT -> reverse . range . swap
           LT -> range)
        (a, b)
  let iterOf = zip `on` range'
  let addToMap m point = Map.insertWith (+) point 1 m
  let addLine pointToCovered line = foldl addToMap pointToCovered $ uncurry iterOf line
  let runOn lines = print $ Map.size $ Map.filter (>= 2) $ foldl addLine Map.empty lines
  let horizOrV ((x1, x2), (y1, y2)) = x1 == x2 || y1 == y2
  runOn $ filter horizOrV ls
  runOn ls
