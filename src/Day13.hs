import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Function
import Data.Tuple
import Util

main :: IO ()
main = getContents
  >>= uncurry (>>) . mapP
  (print . Set.size)
  (putStrLn . toGrid . last)
  . fromJust . uncons . tail
  . (((.) . (uncurry Set.union .) . flip mapP id
      . Set.map . uncurry (updateP . (-) . (2*))
      <*> Set.partition . (uncurry ((. getP) . (.) . (<))))
     . swap & flip & scanl & uncurry)
  . mapP
  (Set.fromList . map ((read :: String -> (Int, Int)) . ("("++) . (++")")))
  (map (mapP axisPSide (read :: String -> Int)
        . listToP . splitOn "=" . drop (length "fold along ")))
  . listToP . map lines . splitOn "\n\n"

main1 = do
  input <- getContents
  let [points', insns'] = map lines $ splitOn "\n\n" input
      insns = mapP axisPSide (read :: String -> Int)
              . listToP . splitOn "="
              . drop (length "fold along ")
              & map $ insns'
      points = Set.fromList $ map ((read :: String -> (Int, Int)) . ("("++) . (++")")) points'
      (_:firstFolded:restFolded) = scanl appFold points insns
  print $ Set.size firstFolded
  putStrLn $ toGrid $ last restFolded
  return ()
  where appFold :: Set.Set (Int, Int) -> (PairSide, Int) -> Set.Set (Int, Int)
        appFold points (side, pos) =
          let (toFold, noFold) = Set.partition ((>pos) . getP side) points
              folded = Set.map (updateP (2*pos-) side) toFold
          in Set.union noFold folded
