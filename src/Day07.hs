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
  let minv = minimum $ map (\al -> sum $ map (abs . subtract al) poss) [minimum poss .. maximum poss]
  print minv
  let tri n = (n * (n + 1)) `div` 2
  let minv = minimum $ map (\al -> sum $ map (tri . abs . subtract al) poss) [minimum poss .. maximum poss]
  print minv
