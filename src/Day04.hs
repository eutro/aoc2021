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
  let boards = map (map (map read . words) . lines) boards' :: [[[Int]]]
  let calledSets = tail $ scanl (flip Set.insert) Set.empty nums
  let winningBoards = tail $ map fst $ scanl (\ (_,bs) set -> partition (hasWon set) bs) ([], boards) calledSets
  let scores = concat $ zipWith3 ((.) map . score) calledSets nums winningBoards
  putStrLn $ show $ head scores
  putStrLn $ show $ last scores
