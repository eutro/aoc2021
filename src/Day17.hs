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
  let [[minx, maxx], [miny, maxy]] = map (map read . splitOn ".." . drop 2) $ splitOn ", " $ drop (length "target area: ") input :: [[Int]]
      area = ((minx, miny), (maxx, maxy))
  print $ last $ map fromJust $ filter isJust $ map (tryY area) [1..100]
  print $ sum $ map (tryY2 area) [miny..100]
  return ()
  where tryY :: ((Int, Int), (Int, Int)) -> Int -> Maybe Int
        tryY area@((minx, miny), (maxx, maxy)) yv =
          case head $ dropWhile ((==LT) . fst) $ map (tryXY area yv) [1..] of
            (EQ, besty) -> Just besty
            _ -> Nothing

        tryY2 :: ((Int, Int), (Int, Int)) -> Int -> Int
        tryY2 area@((minx, miny), (maxx, maxy)) yv =
          length $ filter ((==EQ) . fst) $ map (tryXY area yv) [1..maxx]

        tryXY :: ((Int, Int), (Int, Int)) -> Int -> Int -> (Ordering, Int)
        tryXY area@((minx, miny), (maxx, maxy)) yv xv = loop ((0, 0), (xv, yv)) 0
          where loop s@(pos@(x, y), _) besty
                  | inRange area pos = (EQ, besty)
                  | y < miny && x < minx = (LT, besty)
                  | x > maxx = (GT, besty)
                  | y < miny = (LT, besty)
                  | otherwise = loop (step s) (max y besty)

        step :: SState ->  SState
        step ((x, y), (dx, dy)) =
          let np = (x + dx, y + dy)
              nv = (if dx == 0 then 0 else dx - dx `div` (abs dx), dy - 1)
          in (np, nv)
