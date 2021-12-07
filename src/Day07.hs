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
  let poss = map read $ splitOn "," input :: [Int]
  let distf f al = sum $ map (f . abs . subtract al) poss
  let mindistf = (id >>=
                     (. ((maximum poss &) . (minimum poss &) . binarySearch
                         . (subtract 1 >>=) . on compare)) . ($))
                 . distf
  print $ mindistf id
  print $ mindistf (\n -> (n * (n + 1)) `div` 2)
