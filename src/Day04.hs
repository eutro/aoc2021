import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Util

main :: IO ()
main = getContents
  >>= (id >>= flip (.) (putStrLn . show . last) . (>>) . putStrLn . show . head)
  . uncurry
  (flip $ (.) concat . flip (.) (map (read :: String -> Int) . splitOn ",") . (>>=) id
   . (.) (flip (.) (tail . scanl (flip Set.insert) Set.empty) . (>>=) id)
   . flip (.) (zipWith3 $ (.) map . flip (.) ((. concat) . filter . (.) not . flip Set.member) . (.) . flip (.) sum . (*))
   . (.) . flip (.) . (.) (tail . map fst)
   . scanl (flip (.) ((.) (id >>= flip (.) (any (all id) . transpose) . (||) . any (all id)) . map . map . flip (Set.member))
            . flip partition . snd)
   . curry id []
   . map (map (map (read :: String -> Int) . words) . lines))
  . fromJust . uncons . splitOn "\n\n"

main1 = do
  input <- getContents
  let
    (nums':boards') = splitOn "\n\n" input
    nums = map read $ splitOn "," nums' :: [Int]
    boards = map (map (map read . words) . lines) boards' :: [[[Int]]]
    calledSets = tail $ scanl (flip Set.insert) Set.empty nums
    winningBoards = tail $ map fst $ scanl (\ (_,bs) set -> partition (hasWon set) bs) ([], boards) calledSets
    scores = concat $ zipWith3 ((.) map . score) calledSets nums winningBoards
  putStrLn $ show $ head scores
  putStrLn $ show $ last scores
  where
    hasWon :: (Set.Set Int) -> [[Int]] -> Bool
    hasWon set board =
      let bb = map (map (`Set.member` set)) board
      in any (all id) bb || any (all id) (transpose bb)

    score :: (Set.Set Int) -> Int -> [[Int]] -> Int
    score set called board =
      let uncalled = sum $ filter (not . (`Set.member` set)) $ concat board
      in uncalled * called
