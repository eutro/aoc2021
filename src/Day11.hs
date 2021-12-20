import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.Array
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util hiding (gridBounds)

type Pos = (Int, Int)
type Grid = Array Pos Int

neighbours = flip map (delete (0, 0) $ range ((-1, -1), (1, 1))) . zipPos (+)

gridBounds :: (Pos, Pos)
gridBounds = ((0, 0), (9, 9))

tick :: Grid -> (Grid, Int)
tick grid =
  tick1 Set.empty grid $ indices grid
  where tick1 :: Set.Set Pos -> Grid -> [Pos] -> (Grid, Int)
        tick1 seen grid idcs =
          let (assocs, flashes') = unzip $ do
                (idx, v) <- Map.toList $ frequencies $ filter (`Set.notMember` seen) idcs
                let newv = grid!idx + v
                if inRange gridBounds idx
                  then return ((idx, newv), if newv > 9 then Just idx else Nothing)
                       --                                         ^^^
                       --           guaranteed to be yielded in ascending order
                       --           because it comes from a map
                  else []
              grid' = grid // assocs
              flashes = catMaybes flashes' -- guaranteed to be ascending
          in if null flashes
          then (grid' // (map (flip (,) 0) $ Set.toList seen), Set.size seen)
          else tick1 (Set.union seen $ Set.fromAscList flashes) grid' $ concatMap neighbours flashes
               --                      ^^^^^^^^^^^^^^^ safety: flashes is ascending

main :: IO ()
main = do
  input <- getContents
  let grid = listArray gridBounds $ concat $ map (map digitToInt) $ lines input
      (_:counts) = map snd $ iterate (tick . fst) (grid, 0)
      phase = succ $ length $ takeWhile (/=100) $ counts
      prefixSums = listArray (0, phase) $ scanl (+) 0 counts
      sumBetween 0 x
        | x < phase = prefixSums!x
        | otherwise = (prefixSums!phase) + ((x - phase) `div` 10) * 100
      sumBetween a b = sumBetween 0 b - sumBetween 0 (pred a)
  print $ sumBetween 0 100
  print $ phase
