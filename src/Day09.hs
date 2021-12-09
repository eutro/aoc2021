import qualified Data.Set as Set
import Data.Array
import Data.List
import Data.Char
import Data.Function
import Util

main :: IO ()
main = getContents >>=
  ((((.) . flip (uncurry . if') . flip (,) (const 10) . (!)
     >>= (. (inRange . bounds)) . (.) ((>>=) id)) >>=
    (curry $
     (uncurry
      ((. indices) . filter
       . ((>>=) . ((zip . repeat) .) >>=
          (. ((.) (all (uncurry (<)) .)
              . flip (.) . flip (.)
              (flip map [(-1, 0), (1, 0), (0, -1), (0, 1)] .
               ((uncurry .) . uncurry . (flip .)
                . curry . (curry .) . on (,) . uncurry) (+))
              . map))))) >>=
     (flip $ uncurry $ flip $ const $
      (flip (.) . (. (on (>>) . (&))) . (&) . ((print . sum) .) . map . (.) (+1)) >>=
      (flip (.) $ (&)
       . (.) (print . product . take 3 . sortBy (flip compare))
       . map . (.) Set.size . (Set.empty&) . fix
       . (.) ((>>=) ((>>=) . ((.) . flip if' >>= (. flip Set.member))))
       . (.) (flip (.)) . (.) (flip (.) .)
       . (. ((. flip Set.insert) . (.) . foldl))
       . flip (>>=) . (. (>>=)) . flip (.)
       . ((.) ((.) (. (flip map [(-1, 0), (1, 0), (0, -1), (0, 1)] .
                       ((uncurry .) . uncurry . (flip .)
                        . curry . (curry .) . on (,) . uncurry) (+))))
          . (((flip (.) . filter . conjoin) .)
             . (. flip Set.notMember)
             . (flip (:) . (:[]) . (.) (<9)))))))))
  . ((&) >>=
     ((. concat) . listArray . (,) (0, 0)
      . ((pred . length &) . (pred . length . head &) . on (,))))
  . map (map digitToInt) . lines
