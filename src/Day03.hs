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
  let ls = map (map (=='1')) $ lines input
  let idcs = [0..(length (head ls) - 1)]
  let mostCommon =
        flip (.) (flip (.) (flip (!!)) . flip map)
        . (.) . flip (.) ((* 2) . length . filter id)
        . (<=) . length >>= ($)
  let comb f = putStrLn $ show $ (f id) * (f not)
  comb $ flip (flip (.) (flip (.) . mostCommon) . (.) . (.) readBinBools . flip map) ls idcs
  let filterBits func =
        readBinBools $ head $ foldl
        ((flip (>>=) ($)
          . flip (.)
          ((.) . (>>=) (flip (.) (flip filter) . (.) . ((==) 1 . length >>= if')) ($))
          . (flip (.)
             . ((.) (flip (>>=) ($))
                . flip (.) mostCommon
                . (.) . ((.) (flip (.) (flip (!!))) . flip (.) ((.) . (==)) . (.) . (.)))))
         func)
        ls idcs
  comb filterBits
