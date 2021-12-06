import qualified Data.Map as Map
import Data.Function
import Data.Ix
import Util

main :: IO ()
main = getContents >>=
  (256&) . (80&) . on (>>) . (. curry range 1)
  . ((.) $ print . Map.foldl (+) 0)
  . foldl (flip $ const $ Map.foldlWithKey
           ((. (id >>=
                (. (Map.insertWith (+) . subtract 1))
                . (((Map.insertWith (+) 6 &) . (Map.insertWith (+) 8 &) . on (.) . (&) &) . if')
                . (0==))) . (.) . (&))
           Map.empty)
  . foldl (flip $ (1&) . Map.insertWith (+)) Map.empty
  . map (read :: String -> Int)
  . splitOn ","

main1 = do
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
