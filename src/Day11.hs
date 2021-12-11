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

type Pos = (Int, Int)
type Grid = Array Pos Int

size :: Int
size = 10

gridBounds :: (Pos, Pos)
gridBounds = ((1, 1), (size, size))

neighbours = flip map (delete (0,0) $ range ((-1, -1), (1, 1))) . zipPos (+)

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
                  else []
              grid' = grid // assocs
              flashes = catMaybes flashes'
          in if null flashes
          then (grid' // (map (flip (,) 0) $ Set.toList seen), Set.size seen)
          else tick1 (foldl (flip Set.insert) seen flashes) grid' $ concatMap neighbours flashes

showGrid :: Grid -> String
showGrid grid =
  let ((minx, miny), (maxx, maxy)) = bounds grid
  in do x <- [miny..maxy]
        (do y <- [minx..maxx]
            show $ grid!(x,y)) ++ "\n"

main :: IO ()
main = do
  input <- getContents
  let grid = listArray gridBounds $ concat $ map (map digitToInt) $ lines input
  let (_:states) = iterate (\ (i, (grid, _)) -> (succ i, tick grid)) (0, (grid, 0))
  print $ sum $ map (snd . snd) $ take 100 states
  print $ fst $ head $ filter ((==(size*size)) . snd . snd) $ states
