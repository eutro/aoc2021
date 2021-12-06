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
        then Map.insertWith (+) 8 v (Map.insertWith (+) 6 v m)
        else Map.insertWith (+) (k - 1) v m
  let updateTimers timers = Map.foldlWithKey updateKey Map.empty timers
  let after80 = foldl (\t _ -> updateTimers t) timers [1..80]
  let countFish m = Map.foldl (+) 0 m
  let after256 = foldl (\t _ -> updateTimers t) timers [1..256]
  print $ countFish after80
  print $ countFish after256
