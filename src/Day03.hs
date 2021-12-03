import Data.Ix
import Util

main :: IO ()
main = getContents
  >>= flip
  (flip (($) >>= (.) . ($ (>>)) . (.) . flip (.))
   (flip $ flip (.)
    (flip (.) .
     (flip (.) (flip (.) (flip (!!)) . flip map)
      . (.) . flip (.) ((* 2) . length . filter id)
      . (<=) . length >>= ($)))
    . (.) . (.) readBinBools . flip map))
  (flip
   . flip
   ((.) ((.) (readBinBools . head))
    . foldl
    . (flip (>>=) ($)
       . flip (.)
       ((.) . (>>=) (flip (.) (flip filter) . (.) . ((==) 1 . length >>= if')) ($))
       . (flip (.)
          . ((.) (flip (>>=) ($))
             . flip (.)
             (flip (.) (flip (.) (flip (!!)) . flip map)
              . (.) . flip (.) ((* 2) . length . filter id)
              . (<=) . length >>= ($))
             . (.) . ((.) (flip (.) (flip (!!))) . flip (.) ((.) . (==)) . (.) . (.)))))))
  . (((.) ((.) ((.) (putStrLn . show) . flip (.) (flip ($) not) . (*) . flip ($) id >>= ($)))
       . flip (.) (curry range 0 . (subtract 1) . length . head)
       . flip . flip ($) >>= ($))
     . map (map (=='1'))
     . lines)
