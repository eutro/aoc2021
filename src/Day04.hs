import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import Util

wonp :: (Set.Set Int) -> [[Int]] -> Bool
wonp set board =
  let bb = map (map (`Set.member` set)) board
  in case ((filter (==[True,True,True,True,True]) bb) ++ (filter (==[True,True,True,True,True]) $ transpose bb)) of
    [] -> False
    _ -> True

score :: (Set.Set Int) -> [[Int]] -> Int -> Int
score set board called =
  let flat = concat board
      uS = sum $ filter (not . (`Set.member` set)) flat
  in uS * called

main :: IO ()
main = do
  input <- getContents
  let (nums':boards') = splitOn "\n\n" input :: [String]
  let nums = map read $ splitOn "," nums' :: [Int]
  let boards = map (map (map read . words) . lines . tail) boards' :: [[[Int]]]
  let loop set (n:ns) =
        let newS = Set.insert n set
            unvict = filter (not . wonp newS) boards
        in case unvict of
             [unwon] -> let loop2 set (n:ns) =
                              let newS = Set.insert n set in
                                if wonp newS unwon
                                then putStrLn $ show $ score newS unwon n
                                else loop2 newS ns
                        in loop2 newS ns
             _ -> loop newS ns
      loop set [] = putStrLn "Nobody won"
  loop Set.empty nums
