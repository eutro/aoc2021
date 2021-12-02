import qualified Data.Map as Map
import Data.Maybe
import Util

main :: IO ()
main = getContents
  >>= ($ [("forward",
            (flip (.) (+) . (.)
             . ((.) (flip (.) (+))
                . flip (.) ((.) . curry id)
                . (.)
                . ((.) (flip (>>=) (curry id))
                   . flip (.) . (*))))
            >>= ($)),
          ("up", flip (.) (curry id) . (.) . (flip (.) (curry id) . flip (.) . flip (-))),
          ("down", flip (.) (curry id) . (.) . (flip (.) (curry id) . flip (.) . (+)))])
  . ($ [("forward", (.) ((.) (curry id) . curry id) . (+)),
        ("up", flip (.) ((.) (curry id) . curry id) . flip (.) . flip (-)),
        ("down", flip (.) ((.) (curry id) . curry id) . flip (.) . (+))])
  . (($) >>= (.) . ($ (>>)) . (.) . flip (.))
  . flip
  (((.) (putStrLn . show . uncurry (*) . fst))
   . flip foldl ((0, 0), 0)
   . (uncurry .)
   . flip . ((.) (uncurry . uncurry . (.) ((.) flip . flip) . flip . fromJust))
   . flip Map.lookup
   . Map.fromList)
  . (flip (.) (map ((read :: String -> Int) . head . tail))
     . zipWith (curry id)
     . map head
     >>= ($))
  . map (splitOn " ")
  . lines

main1 = do
  input <- getContents
  let insns' = map (splitOn " ") (lines input)
  let insns = zipWith (curry id) (map head insns')
              (map ((read :: String -> Int) . head . tail) insns')
  solve advance1 insns
  solve advance2 insns
  where solve foldf insns =
          putStrLn $ show $ uncurry (*) $ fst $ foldl foldf ((0, 0), 0) insns
        advance1 ((h, d), a) (dir, x) =
          case dir of
            "forward" -> ((h + x, d), a)
            "up" -> ((h, d - x), a)
            "down" -> ((h, d + x), a)
        advance2 ((h, d), a) (dir, x) =
          case dir of
            "forward" -> ((h + x, d + a * x), a)
            "up" -> ((h, d), a - x)
            "down" -> ((h, d), a + x)
