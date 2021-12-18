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
