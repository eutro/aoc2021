import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import Util

toBStr l = map (\x -> if x then '1' else '0') l

main :: IO ()
main = do
  input <- getContents
  let ls = map (map (=='1')) $ lines input :: [[Bool]]
  let len = length $ head ls
  let gammal = do
        i <- [0..(len-1)]
        let bs = map (!!i) ls
        let trues = length $ filter id bs
        let falses = length $ filter not bs
        return (trues > falses)
  let epsilonl = map not gammal
  putStrLn $ map (\x -> if x then '1' else '0') gammal
  putStrLn $ map (\x -> if x then '1' else '0') epsilonl
  let oxy = head $ foldl
            (\ls i ->
               if length ls == 1
               then ls
               else let bs = map (!!i) ls
                        trues = length $ filter id bs
                        falses = length $ filter not bs
                        mc = trues >= falses
                    in filter ((== mc) . (!!i)) ls)
            ls [0..(len-1)]
  let co = head $ foldl
            (\ls i ->
               if length ls == 1
               then ls
               else let bs = map (!!i) ls
                        trues = length $ filter id bs
                        falses = length $ filter not bs
                        lc = trues < falses
                    in filter ((== lc) . (!!i)) ls)
            ls [0..(len-1)]
  putStrLn $ toBStr oxy
  putStrLn $ toBStr co
