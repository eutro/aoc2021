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

