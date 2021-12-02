import qualified Data.Map as Map
import Data.Maybe
import Util

main :: IO ()
main = getContents
  >>= ($ [("forward", -- \ x h d a -> ((h + x, d + a * x), a)
            (flip (.) (+) . (.)
             . ((.) (flip (.) (+))
                . flip (.) ((.) . curry id)
                . (.)
                . ((.) (flip (>>=) (curry id))
                   . flip (.) . (*))))
            >>= ($)),
          ("up", -- \ x h d a -> ((h, d), a - x)
            flip (.) (curry id) . (.) . (flip (.) (curry id) . flip (.) . flip (-))),
          ("down", -- \ x h d a -> ((h, d), a + x)
            flip (.) (curry id) . (.) . (flip (.) (curry id) . flip (.) . (+)))])
  . ($ [("forward", -- \ x h d a -> ((h + x, d), a)
          (.) ((.) (curry id) . curry id) . (+)),
        ("up", -- \ x h d a -> ((h, d - x), a)
          flip (.) ((.) (curry id) . curry id) . flip (.) . flip (-)),
        ("down", -- \ x h d a -> ((h, d + x), a)
          flip (.) ((.) (curry id) . curry id) . flip (.) . (+))])
  . (($) >>= (.) . ($ (>>)) . (.) . flip (.))
  . flip
  (((.) (putStrLn . show . uncurry (*) . fst))
   . flip foldl ((0, 0), 0)
   . (uncurry .)
   . flip . ((.) (uncurry . uncurry
                  . (.) ((.) flip . flip) . flip
                  . fromJust))
   . flip Map.lookup
   . Map.fromList)
  . (flip (.) (map ((read :: String -> Int) . head . tail))
     . zipWith (curry id)
     . map head
     >>= ($))
  . map (splitOn " ")
  . lines
