import qualified Data.Map as Map
import Control.Monad
import Data.List
import Data.Maybe
import Data.Tuple
import Util

main :: IO ()
main = getContents
  >>= forM_ [10, 40]
  . (.) (print . uncurry subtract . minMax) . (!!)
  . uncurry
  (flip
   . (. ((. frequencies) . iterate
         . ((Map.fromListWith (+) . concatMap (uncurry (map . flip (,)) . swap)) .)
         . (. Map.toList) . map . (`mapP` id)
         . (.) (tail >>= flip zip) . (<*> pToList) . (.) intersperse . (Map.!)))
   . (.) . map . (.) Map.elems
   . (Map.fromListWith (+) .)
   . (. (map (mapP snd id) . Map.toList))
   . (:) . flip (,) 1 . head
   <*> (tail >>= flip zip))
  . mapP id (Map.fromList . map (mapP listToP head . listToP . splitOn " -> ") . tail)
  . fromJust . uncons . lines
          
type Rules = Map.Map (Char, Char) Char
type PairFreqs = Map.Map (Char, Char) Int

main1 = do
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
