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
  >>= forM_ [10, 40]
  . (.) (print . uncurry subtract . minMax) . (!!)
  . uncurry
  (flip
   . (. ((. frequencies) . iterate
         . ((Map.fromListWith (+) . concatMap (uncurry (map . flip (,)) . swap)) .)
         . (. Map.toList) . map . (`mapP` id)
         . (.) (tail >>= flip zip) . (<*> pToList) . (.) intersperse . (Map.!)))
   . (.) . map . (.) Map.elems
   . (Map.fromListWith (+) .)
   . (. (map (mapP snd id) . Map.toList))
   . (:) . flip (,) 1 . head
   <*> (tail >>= flip zip))
  . mapP id (Map.fromList . map (mapP listToP head . listToP . splitOn " -> ") . tail)
  . fromJust . uncons . lines
          
