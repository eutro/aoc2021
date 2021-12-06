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
  let fish = map read $ splitOn "," input :: [Int]
  let timers = foldl (\m f -> Map.insertWith (+) f 1 m) Map.empty fish
  let updateKey m k v =
        if k == 0
        then Map.insertWith (+) 8 v $ Map.insertWith (+) 6 v m
        else Map.insertWith (+) (k - 1) v m
  let after n = foldl (flip $ const $ Map.foldlWithKey updateKey Map.empty) timers [1..n]
  let countFish n = print $ Map.foldl (+) 0 $ after n
  countFish 80
  countFish 256
