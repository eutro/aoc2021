import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Data.List
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Util

main :: IO ()
main = getContents
  >>= forM_ [True, False]
  . ((print . ("start"&)) .) . (Set.empty&) . fix . ((>>=) id .)
  . (. (flip (.)
        . ((((((.) sum) .) .) . (. (flip zipWith . curry (range . swap) True)) . flip (.)) .)
        . (. flip (isUpper . head >>= (Set.insert &) . (const id &) . if'))
        . ((.) . (.) (((sum .) . map) .))))
  . flip (.)
  . (((.) . ((>>=) id .)
      . (. ((.) . ((1&) . if') . (=="end") <*>))
      . (.) . (. flip (.) (pToList . swap . mapP (filter (`notElem` ["start", "end"])) id))
      . flip (.)) .)
  . (. (partition . flip Set.member))
  . flip (.)
  . (Map.!)
  . Map.fromListWith (++)
  . map (mapP id (:[]))
  . (map swap >>= (++))
  . map (listToP . splitOn "-")
  . lines

main1 = do
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
