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
import Util

type Grid = Array (Int, Int) Int

neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x+1, y+1), (x+1, y-1), (x-1, y+1)]

tick :: Grid -> (Grid, Int)
tick grid =
  tick1 Set.empty grid $ Map.fromList $ map (flip (,) 1) $ indices grid
  where tick1 :: (Set.Set (Int, Int)) -> Grid -> (Map.Map (Int, Int) Int) -> (Grid, Int)
        tick1 seen grid idcs =
          let (grid', flashes') = flash idcs grid
              flashes = filter (`Set.notMember` seen) flashes'
          in if null flashes
          then (grid' // (map (flip (,) 0) $ Set.toList seen), Set.size seen)
          else tick1 (foldl (flip Set.insert) seen flashes) grid'
               (Map.fromListWith (+) $ map (flip (,) 1) $ concatMap neighbours flashes)
        flash :: (Map.Map (Int, Int) Int) -> Grid -> (Grid, [(Int, Int)])
        flash idcs grid =
          let (assocs, flashes) = unzip $ do
                (idx, v) <- Map.toList idcs
                let newv = grid!idx + v
                if inRange (bounds grid) idx
                  then return ((idx, newv), if newv > 9 then Just idx else Nothing)
                  else []
          in (grid // assocs, catMaybes flashes)

showGrid :: Grid -> String
showGrid grid =
  let ((minx, miny), (maxx, maxy)) = bounds grid
  in do x <- [miny..maxy]
        (do y <- [minx..maxx]
            show $ grid!(x,y)) ++ "\n"

main :: IO ()
main = do
  input <- getContents
  let grid' = map (map digitToInt) $ lines input :: [[Int]]
  let grid = listArray ((0, 0), (length (head grid') - 1, length grid' - 1)) $ concat grid'
  print $ sum $ map snd $ scanl (\ (grid, _) _ -> tick grid) (grid, 0) [1..100]
  let allflash = length $ indices grid
  print $ fst $ head $ filter ((==allflash) . snd . snd) $ iterate (\ (i, (grid, _)) -> (succ i, tick grid)) (0, (grid, 0))
