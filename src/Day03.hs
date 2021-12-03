import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import Util

main :: IO ()
main = do
  input <- getContents
  let ls = map (map (=='1')) $ lines input :: [[Bool]]
  let len = length $ head ls
  let mostCommon ls i =
        let bs = map (!!i) ls
            trues = length $ filter id bs
            falses = length $ filter not bs
        in trues >= falses
  let comb f = putStrLn $ show $ (f id) * (f not)
  comb (\f -> readBinBools $ map (f . mostCommon ls) [0..(len-1)])
  let filterBits f =
        readBinBools $ head $ foldl
        (\ls i ->
            if length ls == 1
            then ls
            else filter (f . (== (mostCommon ls i)) . (!!i)) ls)
        ls [0..(len-1)]
  comb filterBits
