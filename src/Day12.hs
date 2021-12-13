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
  let edgeM = Map.fromListWith (++)
              $ map (mapP id (:[]))
              $ (map swap >>= (++))
              $ map (listToP . splitOn "-")
              $ lines input
      allP = print . ("start"&) . allPaths edgeM Set.empty
  mapM_ allP [True, False]
  where
    isBig (c:_) = isUpper c
    allPaths :: Map.Map String [String] -> Set.Set String -> Bool -> String -> Int
    allPaths edgeM = loop
      where loop :: Set.Set String -> Bool -> String -> Int
            loop _ _ "end" = 1
            loop seen noRepeat from =
              let newSeen = if isBig from then seen else Set.insert from seen
                  (succr', succs) = partition (`Set.member` seen) $ edgeM Map.! from
                  succr = filter (`notElem` ["start", "end"]) succr'
              in sum $ zipWith ((sum .) . map . loop newSeen) [noRepeat..] [succs, succr]
