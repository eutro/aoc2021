import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

main :: IO ()
main = do
  input <- getContents
  let ls = map (map (splitOn " ") . splitOn " | ") $ lines input
  print $ length $ filter ((`Set.member` (Set.fromList [2,4,3,7])) . length) $ concat $ map (!!1) ls
  print $ sum $ map solve ls
  where solve [signals, outputs] =
          let schema = [(1, const $ (==2) . Set.size),
                        (4, const $ (==4) . Set.size),
                        (7, const $ (==3) . Set.size),
                        (8, const $ (==7) . Set.size),
                        (9, Set.isSubsetOf . (Map.! 4)),
                        (0, (.) ((Set.isSubsetOf . (Map.! 7) &)
                                 . (const ((==6) . Set.size) &))
                            -- don't worry about this :)
                            . (. (&)) . on . on (&&) . (&) & flip),
                        (3, Set.isSubsetOf . (Map.! 7)),
                        (6, const $ (==6) . Set.size),
                        (5, flip Set.isSubsetOf . (Map.! 6)),
                        (2, const $ const True)]
              step (m, ss) (n, pred) =
                let ([ns], rss) = partition (pred m) ss in
                  (Map.insert n ns m, rss)
              (int2set, []) = foldl step (Map.empty, map Set.fromList signals) schema
              set2int = Map.fromList $ map swap $ Map.toList int2set
              digits = map ((set2int Map.!) . Set.fromList) outputs
              n = readBase 10 digits
          in n
