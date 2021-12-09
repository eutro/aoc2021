import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.Array
import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

main :: IO ()
main = do
  input <- getContents
  let grid' = map (map digitToInt) $ lines input :: [[Int]]
  let height = length grid'
  let width = length (grid'!!0)
  let grid = listArray (0,height-1) $ map (listArray (0,width-1)) grid'
  let inrange x y = not (x < 0 || x >= width || y < 0 || y >= height)
  let get x y = if inrange x y
                then grid!y!x
                else 10
  let lows = do
        y <- [0..(height-1)]
        x <- [0..(width-1)]
        let u = get x (y-1)
        let l = get (x-1) y
        let d = get x (y+1)
        let r = get (x+1) y
        let p = get x y
        if p < u && p < l && p < d && p < r
          then [(x, y)]
          else []
  print $ sum $ map ((+1) . uncurry get) lows
  let neighbours x y = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  let addLow (basins, seen) (x, y) =
        let addP (seen, basin) (x, y) =
              if (x, y) `Set.member` seen
              then (seen, basin)
              else
                let nbs = do
                      nb@(ox, oy) <- neighbours x y
                      if nb `Set.member` seen
                        then []
                        else
                        let ov = get ox oy
                            p = get x y in
                          if ov < 9
                          then [nb]
                          else []
                in foldl addP ((x, y) `Set.insert` seen, (x,y) : basin) nbs
            (seen', basin) = addP (seen, []) (x, y)
        in (length basin : basins, seen')
  let basins = fst $ foldl addLow ([], Set.empty) lows
  print $ product $ take 3 $ sortBy (flip compare) basins
