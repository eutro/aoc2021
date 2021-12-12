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

type P2State = Bool

allPaths :: P2State -> M.Map String (S.Set String) -> String -> S.Set String -> String -> [[String]]
allPaths p2r edgeM to seen from
  = let newSeen = if isBig from then seen else S.insert from seen
        (succr', succs) = partition (`S.member` seen) $ S.toList (edgeM M.! from)
        succr = filter (\ s -> s /= "start" && s /= "end") succr'
    in if from == to
       then [[to]]
       else map (from:) $
            if p2r
            then concatMap (allPaths True edgeM to newSeen) succs ++
                 concatMap (allPaths False edgeM to newSeen) succr
            else concatMap (allPaths p2r edgeM to newSeen) succs

main :: IO ()
main = do
  input <- getContents
  let edges = map ((\ [x, y] -> (x, y)) . splitOn "-") $ lines input :: [(String, String)]
      edgeM = M.fromListWith S.union $ map (\ (s, e) -> (s, S.fromList [e])) (edges ++ map swap edges) :: M.Map String (S.Set String)
  print $ length $ allPaths False edgeM "end" S.empty "start"
  print $ length $ allPaths True edgeM "end" S.empty "start"
  return ()
