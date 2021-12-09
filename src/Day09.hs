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

main1 = do
  input <- getContents
  let grid' = map (map digitToInt) $ lines input :: [[Int]]
  let grid = listArray ((0, 0), (length (head grid') - 1, length grid' - 1)) $ concat grid'
  let get pos = if inRange (bounds grid) pos then grid ! pos else 10
  let neighbours (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  let lows = filter
             (\ pos -> all (uncurry (<)) $ zip (repeat $ get pos) $ map get $ neighbours pos)
             $ indices grid
  print $ sum $ map ((+1) . get) lows
  let findBasin pos =
        Set.size $
        (fix $ \ addP basin pos ->
            if pos `Set.member` basin
            then basin
            else foldl addP (pos `Set.insert` basin)
                 $ filter (conjoin [flip Set.notMember basin, (<9) . get])
                 $ neighbours pos)
        Set.empty pos
  print $ product $ take 3 $ sortBy (flip compare) $ map findBasin lows
