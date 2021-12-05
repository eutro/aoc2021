import qualified Data.Map as Map
import Data.List
import Data.Function
import Data.Ix
import Data.Tuple
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
         . ("(("++) . (++"))") . intercalate "),("
         . map (intercalate ",") . transpose
         . map (splitOn ",") . splitOn " -> ")
  . lines
