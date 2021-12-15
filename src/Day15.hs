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
      gr@(topLeft, bottomRight) = ((1, 1), (length $ head ls, length ls))
      (maxx, maxy) = bottomRight
      bottomRight' = (mapP (*5) (*5) bottomRight)
      nb = (topLeft, bottomRight')
      grid = listArray gr $ concat ls :: Grid

  print $ dijkstras (\ pos -> if inRange (bounds grid) pos then Just (grid!pos) else Nothing)
    bottomRight Set.empty $ Set.fromList [(0, topLeft)]

  print $ dijkstras
    (\ pos@(x, y) ->
       if inRange nb pos
       then Just $ let
         tilex = (x - 1) `div` maxx
         tiley = (y - 1) `div` maxy
         diff = tilex + tiley
         nx = (x - 1) `mod` maxx + 1
         ny = (y - 1) `mod` maxy + 1
         v = grid!(nx, ny)
       in (v + diff - 1) `mod` 9 + 1
       else Nothing)
    bottomRight' Set.empty $ Set.fromList [(0, topLeft)]

  return ()
  where dijkstras :: (Pos -> Maybe Int) -> Pos -> Set.Set Pos -> Set.Set (Int, Pos) -> Int
        dijkstras grid end seen queue =
          let (next', rest) = Set.splitAt 1 queue
              [(nextRisk, nextPos)] = Set.toList next'
          in if nextPos == end then nextRisk
          else if nextPos `Set.member` seen then dijkstras grid end seen rest
          else let seen' = nextPos `Set.insert` seen
                   queue' = Set.union queue
                           $ Set.fromList
                           $ map (\ p -> (nextRisk + (fromJust $ grid p), p))
                           $ filter (\ p -> p `Set.notMember` seen && isJust (grid p))
                           $ neighbours nextPos
               in dijkstras grid end seen' queue'
