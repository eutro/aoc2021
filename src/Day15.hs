import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
import Data.Array
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

type Pos = (Int, Int)
type Grid = Array Pos Int

neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

main :: IO ()
main = do
  input <- getContents
  let ls = map (map digitToInt) $ lines input
      gridSize@(w, h) = (length $ head ls, length ls)
      grid = listArray ((0, 0), (w-1, h-1)) $ concat ls :: Grid
      getPos = succ . (`mod` 9) . pred .
               ((+) . (grid!) . zipPos (flip mod) gridSize
                <*> uncurry (+) . zipPos (flip div) gridSize)
  forM_ [1, 5]
    $ print . dijkstras getPos . (,) (0, 0)
    . (gridSize&) . (id >>= mapP) . (pred .) . (*)
  where dijkstras :: (Pos -> Int) -> (Pos, Pos) -> Int
        dijkstras grid bounds@(start, end) =
          fromLeft 0 $ iterateM step (Set.empty, Set.fromList [(0, start)])
          where step (seen, queue)
                  | nextPos == end = Left nextRisk
                  | nextPos `Set.member` seen = Right (seen, rest)
                  | otherwise = Right (nextPos `Set.insert` seen,
                                       Set.union queue
                                       $ Set.fromList
                                       [(nextRisk + grid p, p)
                                       | p <- neighbours nextPos,
                                         p `Set.notMember` seen,
                                         inRange bounds p])
                  where ((nextRisk, nextPos), rest) = Set.deleteFindMin queue

