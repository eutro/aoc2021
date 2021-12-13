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

-- appFold :: (String, Int) -> [(Int, Int)] -> [(Int, Int)]
appFold points ("x", fx) = Set.toList $ Set.fromList $ [if x > fx then (2*fx - x, y) else (x, y) | (x, y) <- points]
appFold points ("y", fy) = Set.toList $ Set.fromList $ [if y > fy then (x, 2*fy - y) else (x, y) | (x, y) <- points]

main :: IO ()
main = do
  input <- getContents
  let [points', insns'] = splitOn "\n\n" input
      insns = map ((\ [a, b] -> (a, read b)) . splitOn "=" . drop (length "fold along ")) $ lines insns' :: [(String, Int)]
      points = map ((read :: String -> (Int, Int)) . ("("++) . (++")")) $ lines points'
  print $ length $ appFold points (head insns)
  let fullFolded = Set.fromList $ foldl appFold points insns
      (minx, miny) = minimum fullFolded
      (maxx, maxy) = maximum fullFolded
  putStrLn $ unlines [[if (x, y) `Set.member` fullFolded then '#' else '.' | x <- [minx..maxx]] | y <- [miny..maxy]]
  return ()
