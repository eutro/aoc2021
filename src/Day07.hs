import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

main :: IO ()
main = getContents
  >>= (((`div` 2) . ((+1) >>= (*)) &) . (id&) . on (>>))
  . (.) print
  . (. (. subtract) . (.) . (. abs))
  . ((.) . ((>>=) id
            . ((. ($)) .
               flip (.) .
               (. (binarySearch . (subtract 1 >>=) . on compare))
               . (id >>= (. ((&) . minimum)) . (.) . ((&) . maximum))))
     >>= (. ((.) . (((sum .) . (. map)) . (&)))))
  . map (read :: String -> Int)
  . splitOn ","
