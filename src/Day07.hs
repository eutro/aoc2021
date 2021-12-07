import Data.Function
import Util

main :: IO ()
main = getContents
  >>= (((`div` 2) . ((+1) >>= (*)) &) . (id&) . on (>>))
  . (.) print
  . (. (. subtract) . (.) . (. abs))
  . ((.) . ((>>=) id
            . ((. ($)) .
               flip (.) .
               (. (binarySearch . (subtract 1 >>=) . on compare))
               . (id >>= (. ((&) . minimum)) . (.) . ((&) . maximum))))
     >>= (. ((.) . (((sum .) . (. map)) . (&)))))
  . map (read :: String -> Int)
  . splitOn ","

main1 = do
  input <- getContents
  let poss = map read $ splitOn "," input :: [Int]
  let (minv, maxv) = on (,) (poss&) minimum maximum
  let distf f al = sum $ map (f . abs . subtract al) poss
  let mindistf f =
        distf f $ binarySearch (subtract 1 >>= on compare (distf f)) minv maxv
        -- minimum $ map (distf f) [minv..maxv]
  print $ mindistf id
  print $ mindistf (\n -> (n * (n + 1)) `div` 2)
