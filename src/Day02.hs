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
  let insns = map (splitOn " ") (lines input)
  loop 0 0 insns
  loop2 0 0 0 insns
  where loop horiz depth [] = putStrLn $ show (horiz * depth)
        loop horiz depth ([dir, mag]:tail) =
          let magn = read mag :: Int in
            case dir of
              "forward" -> loop (horiz + magn) depth tail
              "up" -> loop horiz (depth - magn) tail
              "down" -> loop horiz (depth + magn) tail
        loop2 horiz depth aim [] = putStrLn $ show (horiz * depth)
        loop2 horiz depth aim ([dir, mag]:tail) =
          let magn = read mag :: Int in
            case dir of
              "forward" -> loop2 (horiz + magn) (depth + aim * magn) aim tail
              "up" -> loop2 horiz depth (aim - magn) tail
              "down" -> loop2 horiz depth (aim + magn) tail
