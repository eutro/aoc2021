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
      gridSize = (length $ head ls, length ls)
      gr@(topLeft, bottomRight) = ((0, 0), mapP pred pred gridSize)
      gridSize' = (mapP (*5) (*5) gridSize)
      nb@(_, bottomRight') = (topLeft, mapP pred pred gridSize')
      grid = listArray gr $ concat ls :: Grid
      getPos pos = succ
                   $ (`mod` 9)
                   $ pred
                   $ grid!(zipPos mod pos gridSize) +
                   (uncurry (+) $ zipPos div pos gridSize)
  mapM_ (print . uncurry dijkstras) [(gr, (grid!)), (nb, getPos)]
  where dijkstras :: (Pos, Pos) -> (Pos -> Int) -> Int
        dijkstras bounds@(start, end) grid = loop Set.empty $ Set.fromList [(0, start)]
          where loop seen queue =
                  let (next', rest) = Set.splitAt 1 queue
                      [(nextRisk, nextPos)] = Set.toList next'
                  in if nextPos == end then nextRisk
                  else if nextPos `Set.member` seen then loop seen rest
                  else let seen' = nextPos `Set.insert` seen
                           queue' = Set.union queue
                             $ Set.fromList
                             $ map (\ p -> (nextRisk + grid p, p))
                             $ filter (\ p -> p `Set.notMember` seen && inRange bounds p)
                             $ neighbours nextPos
                  in loop seen' queue'
