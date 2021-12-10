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

checkC :: String -> Either Char String
checkC s =
  check' s []
  where check' (')':s) (')':stack) = check' s stack
        check' (']':s) (']':stack) = check' s stack
        check' ('}':s) ('}':stack) = check' s stack
        check' ('>':s) ('>':stack) = check' s stack
        check' ('(':s) stack = check' s (')':stack)
        check' ('[':s) stack = check' s (']':stack)
        check' ('{':s) stack = check' s ('}':stack)
        check' ('<':s) stack = check' s ('>':stack)
        check' [] stack = Right stack
        check' (c:s) stack = Left c

score1 = (Map.!) $ Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
score2 = (Map.!) $ Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

main :: IO ()
main = do
  input <- getContents
  let (corrupt, incomplete) = partitionEithers $ map checkC $ lines input
  print $ sum $ map score1 corrupt
  print $ (\ l -> l !! (length l `div` 2)) $ sort $ map (readBase 5 . map score2) incomplete
