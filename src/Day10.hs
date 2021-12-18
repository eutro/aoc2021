import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Applicative
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

open2close :: Map.Map Char Char
open2close = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

checkC :: String -> Either Char String
checkC s = foldM step [] s
  where step stack c =
          Right . (:stack) <$> Map.lookup c open2close
          <|> do (match, porridge) <- uncons stack
                 guard (c == match)
                 Just $ Right porridge
          & fromMaybe (Left c) 

score1 = (Map.!) $ Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]
score2 = (Map.!) $ Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]

main :: IO ()
main = do
  input <- getContents
  let (corrupt, incomplete) = partitionEithers $ map checkC $ lines input
  print $ sum $ map score1 corrupt
  print $ (\ l -> l !! (length l `div` 2)) $ sort $ map (readBase 5 . map score2) incomplete
