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

type Rules = Map.Map (Char, Char) Char
type PairFreqs = Map.Map (Char, Char) Int

main :: IO ()
main = do
  input <- getContents
  let (template : "" : rules') = lines input
      rules = Map.fromList $ map (mapP listToP head . listToP . splitOn " -> ") rules' :: Rules
      growth = map (Map.elems . countChars template) $ iterate (appRules rules) $ countPairs template
      solveN n = print $ uncurry subtract $ minMax $ growth !! n
  mapM_ solveN [10, 40]
  where appRules :: Rules -> PairFreqs -> PairFreqs
        appRules =
          ((Map.fromListWith (+) . concatMap (uncurry (map . flip (,)) . swap)) .)
          . (. Map.toList) . map . (`mapP` id) . decomp
        countChars :: String -> PairFreqs -> Map.Map Char Int
        countChars = (Map.fromListWith (+) .) . (. (map (mapP snd id) . Map.toList)) . (:) . flip (,) 1 . head
        countPairs :: String -> PairFreqs
        countPairs = frequencies . (tail >>= flip zip)
        decomp :: Rules -> (Char, Char) -> [(Char, Char)]
        decomp = (.) (tail >>= flip zip) . (<*> pToList) . (.) intersperse . (Map.!)
