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
main = getContents
  >>= (id&) . (&) (filter $ uncurry $ on (||) $ uncurry (==)) . on (>>)
  . (.) (print . Map.size . Map.filter (>=2)
         . foldl ((. (uncurry $ on zip $ fromEnum . uncurry compare
                       >>= ([range, repeat . fst, reverse . range . swap]!!)))
                  . foldl (flip ((1&) . Map.insertWith (+)))) Map.empty)
  . (&)
  . map ((read :: String -> ((Int, Int), (Int, Int)))
         . ("(("++) . (++"))")  -- "((x1,x2),(y1,y2))"
         . intercalate "),(" -- "x1,x2),(y1,y2"
         . map (intercalate ",") -- ["x1,x2", "y1,y2"]
         . transpose -- [["x1", "x2"], ["y1", "y2"]]
         . map (splitOn ",") -- [["x1", "y1"], ["x2", "y2"]]
         . splitOn " -> ") -- ["x1,y1", "x2,y2"]
  . lines
