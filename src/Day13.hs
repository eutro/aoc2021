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

main :: IO ()
main = do
  input <- getContents
  let [points', insns'] = splitOn "\n\n" input
      insns = map (mapP axisPSide (read :: String -> Int)
                   . listToP
                   . splitOn "="
                   . drop (length "fold along "))
              $ lines insns'
      points = Set.fromList
               $ map ((read :: String -> (Int, Int)) . ("("++) . (++")"))
               $ lines points'
      (_:firstFolded:restFolded) = scanl appFold points insns
  print $ Set.size firstFolded
  putStrLn $ toGrid $ last restFolded
  return ()
  where appFold :: Set.Set (Int, Int) -> (PairSide, Int) -> Set.Set (Int, Int)
        appFold points (side, pos) =
          let (toFold, noFold) = Set.partition ((>pos) . getP side) points
              folded = Set.map (updateP (2*pos-) side) toFold
          in Set.union noFold folded
        toGrid :: Set.Set (Int, Int) -> String
        toGrid points = let
          (minx, miny) = minimum points
          (maxx, maxy) = maximum points
          in unlines
             [[if (x, y) `Set.member` points
               then '#'
               else ' '
              | x <- [minx..maxx]]
             | y <- [miny..maxy]]
