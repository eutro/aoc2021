import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
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
main = getContents
  >>= forM_ [True, False]
  . ((print . ("start"&)) .)
  . ((Set.empty&) . allPaths)
  . Map.fromListWith (++)
  . map (mapP id (:[]))
  . (map swap >>= (++))
  . map (listToP . splitOn "-")
  . lines
  where
    allPaths :: Map.Map String [String] -> Set.Set String -> Bool -> String -> Int
    allPaths edgeM = fix $ \ loop seen noRepeat from ->
      if' ((=="end") from) 1
      $ sum $ zipWith
      ((sum .) . map . loop
       ((isUpper . head >>= (Set.insert &) . (const id &) . if')
        from seen))
      [noRepeat..]
      $ pToList
      $ swap
      $ mapP (filter (`notElem` ["start", "end"])) id
      $ partition (`Set.member` seen) $ edgeM Map.! from
