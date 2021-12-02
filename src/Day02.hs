import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import Util

main :: IO ()
main = getContents
  >>= ($ [("forward", \h d a x -> ((h + x, d + a * x), a)),
          ("up", \h d a x -> ((h, d), a - x)),
          ("down", \h d a x -> ((h, d), a + x))])
  . ($ [("forward", \h d a x -> ((h + x, d), a)),
        ("up", \h d a x -> ((h, d - x), a)),
        ("down", \h d a x -> ((h, d + x), a))])
  . (($) >>= (.) . ($ (>>)) . (.) . flip (.))
  . flip
  (((.) (putStrLn . show . uncurry (*) . fst))
   . flip foldl ((0, 0), 0)
   . (uncurry .) . flip
   . ((.) (uncurry . uncurry . fromJust))
   . flip Map.lookup
   . Map.fromList)
  . (flip (.) (map ((read :: String -> Int) . head . tail))
     . zipWith (curry id)
     . map head
     >>= ($))
  . map (splitOn " ")
  . lines
