import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
import Control.Monad.State
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

type SState = ((Int, Int), (Int, Int)) -- pos, vel

main :: IO ()
main = do
  input <- getLine
  let [[minx, maxx], [miny, maxy]] =
        map (map read . splitOn ".." . drop 2)
        $ splitOn ", "
        $ drop (length "target area: ") input :: [[Int]]
      area = ((minx, miny), (maxx, maxy))
      dycap = pred (-miny)
  print $ triangle dycap
  print $ length $ filter (tryXY area) $ range ((1, miny), (maxx, dycap))
  return ()
  where tryXY :: SState -> (Int, Int) -> Bool
        tryXY area@((minx, miny), (maxx, maxy)) vel = loop ((0, 0), vel)
          where loop s@(pos@(x, y), (dx, _))
                  | inRange area pos = True
                  | y < miny || x > maxx = False
                  | dx == 0 && x < minx = False
                  | otherwise = loop (step s)

        step :: SState -> SState
        step ((x, y), (dx, dy)) =
          ((x + dx, y + dy), (dx - sign dx, dy - 1))
