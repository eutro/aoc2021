import qualified Data.Set as Set
import Bits

type Pos = (Int, Int)
type Grid = Array Pos Int

main :: IO ()
main = map (map digitToInt) <$> lines <$> getContents >>=
  ((,) <$> length . head <*> length >>=
   flip (.) . (. concat) . listArray . (,) (0, 0) . mapBoth pred <*>
   ((.) <$>
    (forM_ [1, 5] .)
    . flip (.)
    . ((,) (0, 0) .)
    . (. (.) pred . (*))
    . flip mapBoth
    <*> (.)
    ((.) print . join
     . uncurry
     . flip
     . (flip .)
     . flip
     . (.)
     ((.) ((. Set.singleton . (,) 0) . evalState . flip evalStateT Set.empty . fix)
      . (. (. fromMaybe) . flip (.)
         . (.) (listToMaybe . map (return . fst))
         . filter . (. snd) . (==))
      . (.)
      . ((.) ((lift (state Set.deleteFindMin) >>=) . uncurry)))
     . (. (.)
        . (. (=<<) . ((>>) <$> (modify . Set.union . Set.fromList) <*>))
        . flip (.)
        . (. flip filter
           . flip map [(-1, 0), (1, 0), (0, -1), (0, 1)]
           . addPos)
        . flip fmap
        . (<$> (flip Set.notMember <$> get))
        . (<*>)
        . fmap (&&)
        . inRange)
     . flip (.)
     . (. (.) . ((>>) <$> lift . modify . Set.union . Set.fromList <*>))
     . flip (.)
     . (.) (map . (>>= (,)))
     . (. (+))
     . flip (.))
    . flip
    (flip
     . curry
     . ((`mod1` 9) .)
     . (<*> uncurry (+) . (uncurry $ zipPos div))
     . fmap (+)
     . (. (uncurry $ zipPos mod))
     . (!))))
