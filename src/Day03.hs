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

main1 = do
  input <- getContents
  let ls = map (map (=='1')) $ lines input
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
