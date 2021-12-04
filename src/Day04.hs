import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Util

main :: IO ()
main = getContents
  >>= (id >>= flip (.) (putStrLn . show . last) . (>>) . putStrLn . show . head)
  . uncurry
  (flip $ (.) concat . flip (.) (map (read :: String -> Int) . splitOn ",") . (>>=) id
   . (.) (flip (.) (tail . scanl (flip Set.insert) Set.empty) . (>>=) id)
   . flip (.) (zipWith3 $ (.) map . flip (.) ((. concat) . filter . (.) not . flip Set.member) . (.) . flip (.) sum . (*))
   . (.) . flip (.) . (.) (tail . map fst)
   . scanl (flip (.)((.) (id >>= flip (.) (any (all id) . transpose) . (||) . any (all id)) . map . map . flip (Set.member))
            . flip partition . snd)
   . curry id []
   . map (map (map (read :: String -> Int) . words) . lines))
  . fromJust . uncons . splitOn "\n\n"
  
