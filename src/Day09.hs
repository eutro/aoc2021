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
  let grid = listArray ((0, 0), (length (head grid') - 1, length grid' - 1)) $ concat grid'
  let get pos = if inRange (bounds grid) pos then grid ! pos else 10
  let neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  let lows = filter
             (\ pos -> all (uncurry (<)) $ zip (repeat $ get pos) $ map get $ neighbours pos)
             $ indices grid
  print $ sum $ map ((+1) . get) lows
  let findBasin pos =
        Set.size $
        (fix $ \ addP basin pos ->
            if pos `Set.member` basin
            then basin
            else foldl addP (pos `Set.insert` basin)
                 $ filter (conjoin [flip Set.notMember basin, (<9) . get])
                 $ neighbours pos)
        Set.empty pos
  print $ product $ take 3 $ sortBy (flip compare) $ map findBasin lows
