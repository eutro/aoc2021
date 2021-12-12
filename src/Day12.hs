import qualified Data.Set as S
import qualified Data.Map as M
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

isBig (c:_) = isUpper c

allPaths :: String -> M.Map String [String] -> Bool -> S.Set String -> String -> [[String]]
allPaths to edgeM = fix $ \ recur allowRepeat seen from ->
  if from == to
  then [[to]]
  else let
    newSeen = if isBig from then seen else S.insert from seen
    (succr', succs) = partition (`S.member` seen) $ edgeM M.! from
    succr = filter (\ s -> s /= "start" && s /= "end") succr'
  in map (from:) $
     concatMap (recur allowRepeat newSeen) succs ++
     if allowRepeat
     then concatMap (recur False newSeen) succr
     else []

main :: IO ()
main = do
  input <- getContents
  let edges = map ((\ [x, y] -> (x, y)) . splitOn "-") $ lines input :: [(String, String)]
      edgeM = M.fromListWith (++) $ map (\ (s, e) -> (s, [e])) (edges ++ map swap edges) :: M.Map String [String]
      allP = allPaths "end" edgeM
  print $ length $ allP False S.empty "start"
  print $ length $ allP True S.empty "start"
  return ()
