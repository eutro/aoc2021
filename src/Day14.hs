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

main :: IO ()
main = do
  input <- getContents
  let (template : _ : rules') = lines input
      rules = Map.fromList $ map ((\ [[a, b], [rhs]] -> ((a, b), rhs)) . splitOn " -> ") rules' :: Map.Map (Char, Char) Char
      growth = iterate (appRules rules) $ parsePairFreqs template
      solveN n =
        let fAtN = Map.toList $ pairFreqsToCharC (growth!!n) template
            (minEl, minElc) = minimumBy (compare `on` snd) fAtN
            (maxEl, maxElc) = maximumBy (compare `on` snd) fAtN
        in print (maxElc - minElc)
  solveN 10
  solveN 40
  return ()
  where parsePairFreqs :: String -> Map.Map (Char, Char) Int
        parsePairFreqs s = frequencies $ zip s (tail s)
        appRules :: Map.Map (Char, Char) Char
                 -> Map.Map (Char, Char) Int
                 -> Map.Map (Char, Char) Int
        appRules rules s =
          Map.fromListWith (+) $ concat [[(sub, freq) | sub <- decomposition rules rule] | (rule, freq) <- Map.toList s]

        pairFreqsToCharC :: Map.Map (Char, Char) Int -> String -> Map.Map Char Int
        pairFreqsToCharC pairs s = Map.fromListWith (+) $ (last s, 1) : [(l, freq) | ((l, _), freq) <- Map.toList pairs]

        decomposition :: Map.Map (Char, Char) Char -> (Char, Char) -> [(Char, Char)]
        decomposition rules rule@(a, b) =
          let c = rules Map.! rule
          in [(a, c), (c, b)]
