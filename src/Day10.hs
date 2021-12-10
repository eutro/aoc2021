import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.Array
import Data.List
import Data.Maybe
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

main :: IO ()
main = do
  input <- getContents
  let ls = lines input
  print $ sum $ do
    l <- ls
    case checkC l of
      Left c -> return $ case c of    
                           ')' -> 3
                           ']' -> 57
                           '}' -> 1197
                           '>' -> 25137
      Right _ -> []

  let ws = do
        l <- ls
        case checkC l of
          Left _ -> []
          Right f -> return $ readBase 5
                     $ map (\ c -> case c of
                                     ')' -> 1
                                     ']' -> 2
                                     '}' -> 3
                                     '>' -> 4) f
  let ws' = sort ws
  print $ ws' !! (length ws' `div` 2)
