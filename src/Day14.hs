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
        appRules rules freqs = Map.fromListWith (+) $
          do (rule, freq) <- Map.toList freqs
             sub <- decomp rules rule
             return (sub, freq)
        countChars :: String -> PairFreqs -> Map.Map Char Int
        countChars (c:_) pairs = Map.fromListWith (+) $ (c, 1) : (map (mapP snd id) $ Map.toList pairs)
        countPairs :: String -> PairFreqs
        countPairs s@(_:r) = frequencies $ zip s r
        decomp :: Rules -> (Char, Char) -> [(Char, Char)]
        decomp rules rule@(a, c) = [(a, b), (b, c)] where b = rules Map.! rule
