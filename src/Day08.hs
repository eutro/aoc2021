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
  let outs = concat $ map (!!1) ls
  print $ length $ filter ((`Set.member` (Set.fromList [2,4,3,7])) . length) outs
  let solve [signals, outputs] =
        let sass = map Set.fromList signals
            [svn] = filter ((==3) . Set.size) sass
            [one] = filter ((==2) . Set.size) sass
            [four] = filter ((==4) . Set.size) sass
            [eight] = filter ((==7) . Set.size) sass
            sass' = filter ((`Set.notMember` (Set.fromList [2,4,3,7])) . length) sass
            bd = Set.difference four one
            n023 = filter (not . (bd `Set.isSubsetOf`)) sass'
            [zero] = filter ((==6) . Set.size) n023
            n23 = filter ((==5) . Set.size) n023
            [two] = filter (not . (one `Set.isSubsetOf`)) n23
            [three] = filter (one `Set.isSubsetOf`) n23
            n569 = filter (bd `Set.isSubsetOf`) sass'
            [five] = filter ((==5) . Set.size) n569
            [nine] = filter (one `Set.isSubsetOf`) n569
            n56 = filter (not . (one `Set.isSubsetOf`)) n569
            [six] = filter ((==6) . Set.size) n56
            digitmap = Map.fromList [(zero, 0),
                                     (one, 1),
                                     (two, 2),
                                     (three, 3),
                                     (four, 4),
                                     (five, 5),
                                     (six, 6),
                                     (svn, 7),
                                     (eight, 8),
                                     (nine, 9)]
            digits = map (fromJust . (`Map.lookup` digitmap) . Set.fromList) outputs
            n = readBase 10 digits
        in n
  print $ sum $ map solve ls
