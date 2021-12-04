import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import Util

hasWon :: (Set.Set Int) -> [[Int]] -> Bool
hasWon set board =
  let bb = map (map (`Set.member` set)) board
  in any (all id) bb || any (all id) (transpose bb)

score :: (Set.Set Int) -> Int -> [[Int]] -> Int
score set called board =
  let flat = concat board
      uS = sum $ filter (not . (`Set.member` set)) flat
  in uS * called

main :: IO ()
main = do
  input <- getContents
  let (nums':boards') = splitOn "\n\n" input :: [String]
  let nums = map read $ splitOn "," nums' :: [Int]
  let boards = map (map (map read . words) . lines . tail) boards' :: [[[Int]]]
  let loop set boards (n:ns) =
        let newS = Set.insert n set
            (vict, unvict) = partition (hasWon newS) boards
        in map (score newS n) vict ++ loop newS unvict ns
      loop set [] ns = []
      loop set boards [] = []
  let scores = loop Set.empty boards nums
  putStrLn $ show $ head scores
  putStrLn $ show $ last scores
