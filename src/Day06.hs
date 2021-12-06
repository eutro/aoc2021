import qualified Data.Map as Map
import Data.Function
import Data.Ix
import Util
-- import Matrix

main :: IO ()
main = getContents
  >>= ([6703087164, 6206821033, 5617089148, 5217223242, 4726100874, 4368232009]&)
  . ([1421, 1401, 1191, 1154, 1034, 950]&)
  . on (>>) . ((print . sum) .) . (. (map . (!!))) . (&)
  . map (read :: String -> Int) . splitOn ","

-- sumMat = [[1,1,1,1,1,1,1,1,1]]
-- stepMat = [[0,1,0,0,0,0,0,0,0],
--            [0,0,1,0,0,0,0,0,0],
--            [0,0,0,1,0,0,0,0,0],
--            [0,0,0,0,1,0,0,0,0],
--            [0,0,0,0,0,1,0,0,0],
--            [0,0,0,0,0,0,1,0,0],
--            [1,0,0,0,0,0,0,1,0],
--            [0,0,0,0,0,0,0,0,1],
--            [1,0,0,0,0,0,0,0,0]]
-- solveNVec n = head $ sumMat `mxm` (mpow n stepMat)
-- solveNVec 256 = [6703087164,6206821033,5617089148,
--                  5217223242,4726100874,4368232009,
--                  3989468462,3649885552,3369186778]
-- solveNVec 80 = [1421,1401,1191,1154,1034,950,905,779,768]
-- -- input is in range [0,6), so we only need the first 6 terms

main0 = getContents >>=
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
