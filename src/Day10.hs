import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Either
import Data.Function
import Util

main :: IO ()
main =
  lines <$> getContents >>=
  uncurry
  ((. sum . map (Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)] Map.!))
   $ (. median . map
      (readBase 5 . map (Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)] Map.!)))
   . (on (>>) print))
  . partitionEithers . map
  (foldM
   ((fromMaybe . Left <*>) .
    ((<*>) . ((<|>) .)
     . (. (`Map.lookup` Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]))
     . fmap . (Right .) . flip (:)
     <*>
     (. uncurry . ((.) $ (. Just . Right) . (>>) . guard) . (==))
     . (>>=) . uncons)) [])

main1 = do
  input <- getContents
  let (corrupt, incomplete) = partitionEithers $ map check $ lines input
  print $ sum $ map score1 corrupt
  print $ median $ map (readBase 5 . map score2) incomplete
  where score1 = (Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)] Map.!)
        score2 = (Map.fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)] Map.!)
        open2close :: Map.Map Char Char
        open2close = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]
        check :: String -> Either Char String
        check s = foldM step [] s
          where step stack c =
                  Right . (:stack) <$> Map.lookup c open2close
                  <|> do (match, porridge) <- uncons stack
                         guard (c == match)
                         Just $ Right porridge
                  & fromMaybe (Left c) 
