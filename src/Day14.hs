import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Data.Array
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

-- import qualified Data.Matrix as Mat
-- 
-- main0 = do
--   input <- getContents
--   let (template : "" : rules') = lines input
--       rules = Map.fromList $ map (mapP listToP head . listToP . splitOn " -> ") rules' :: Rules
--       size = Map.size rules
--       matrix = Mat.transpose $ Mat.fromLists $ map (uncurry $ genRow rules) $ Map.toList rules
--       vec = Mat.fromList size 1
--             $ map snd
--             $ Map.toList
--             $ Map.unionWith (+) (Map.map (const 0) rules) (countPairs template)
--       solveN n = let
--         matrix' = matrix ^ n
--         outv = matrix' * vec
--         outFreqs = Map.elems
--                    $ Map.fromListWith (+)
--                    $ (head template, 1)
--                    : zip (map (snd . fst) (Map.toList rules)) (Mat.toList outv)
--         in print $ uncurry subtract (minMax outFreqs)
--   forM_ [10, 40] solveN
--   where genRow rules rule@(l, r) out =
--           map (toInteger . fromEnum . (`elem` outs) . fst) $ Map.toList rules
--           where outs = [(l, out), (out, r)]
--         countPairs s@(_:r) = frequencies $ zip s r
