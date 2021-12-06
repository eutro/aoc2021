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
main = getContents >>=
  (256&) . (80&) . on (>>) . (. curry range 1)
  . ((.) $ print . Map.foldl (+) 0)
  . foldl (flip $ const $ Map.foldlWithKey
           ((. (id >>=
                (. (Map.insertWith (+) . subtract 1))
                . (((Map.insertWith (+) 6 &) . (Map.insertWith (+) 8 &) . on (.) . (&) &) . if')
                . (0==))) . (.) . (&))
           Map.empty)
  . foldl (flip $ (1&) . Map.insertWith (+)) Map.empty
  . map (read :: String -> Int)
  . splitOn ","
