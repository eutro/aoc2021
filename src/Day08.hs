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
main = getContents >>=
  (print . sum . map
   (id >>=
    (.) (readBase 10) . (. (head . tail))
    . map . (. Set.fromList) . (Map.!)
    . Map.fromList . map swap . Map.toList . fst
    . (([(1, const $ (==2) . Set.size),
         (4, const $ (==4) . Set.size),
         (7, const $ (==3) . Set.size),
         (8, const $ (==7) . Set.size),
         (9, Set.isSubsetOf . (Map.! 4)),
         (0, (.) ((Set.isSubsetOf . (Map.! 7) &)
                  . (const ((==6) . Set.size) &))
             . (. (&)) . on . on (&&) . (&) & flip),
         (3, Set.isSubsetOf . (Map.! 7)),
         (6, const $ (==6) . Set.size),
         (5, flip Set.isSubsetOf . (Map.! 6)),
         (2, const $ const True)]&)
       . foldl
       (uncurry
        $ flip (.) ((flip (.) .) . (. flip partition) . flip (.) . (&))
        . (.) . flip (.) . (. Map.insert)
        . (.) (uncurry . (. head))
        . (.) . (.) (,) . (&)
        >>= ((.) uncurry .)))
    . (,) Map.empty . map Set.fromList . head) &)
  . (print . length . filter
     ((`Set.member` (Set.fromList [2,4,3,7])) . length)
     . concat . map (!!1) &)
  . on (>>) . (&)
  . map (map (splitOn " ") . splitOn " | ") . lines
