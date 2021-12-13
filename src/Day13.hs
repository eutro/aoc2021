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

main :: IO ()
main = getContents
  >>= uncurry (>>) . mapP
  (print . Set.size)
  (putStrLn . toGrid . last)
  . fromJust . uncons . tail
  . (((.) . (uncurry Set.union .) . flip mapP id
      . Set.map . uncurry (updateP . (-) . (2*))
      <*> Set.partition . (uncurry ((. getP) . (.) . (<))))
     . swap & flip & scanl & uncurry)
  . mapP
  (Set.fromList . map ((read :: String -> (Int, Int)) . ("("++) . (++")")))
  (map (mapP axisPSide (read :: String -> Int)
        . listToP . splitOn "=" . drop (length "fold along ")))
  . listToP . map lines . splitOn "\n\n"
