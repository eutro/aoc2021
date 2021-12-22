import qualified Data.Set as Set
import qualified Data.Map as Map
import Bits hiding (gridBounds)

type Pos = (Int, Int)
type Grid = Array Pos Int

main :: IO ()
main =
  map (map digitToInt) <$> lines <$> getContents >>=
  ((>>) <$> print . sum . take 100 <*> print . succ . length . takeWhile (/=100))
  . evalState
  (sequence $ repeat $
   (fix
    $ (. (<$> lift get)
       . ((mapRight catMaybes . unzip) .)
       . (. ((>>) <$> ((guard . inRange ((0, 0), (9, 9))) . fst) <*>)
          . (>>= flip ((. fst) $ curry $ (return <$>) $ (,) <$> id
                       <*> uncurry (flip $ (. Just) . (>>) . guard . (>9))))
          . uncurry . ((+) .) . (!))
        . ((>>=) . Map.toList . frequencies))
    . flip (>>=)
    . (if' <$> (null . snd)
       <*> (>> Set.size <$> get) . (map (flip (,) 0) . Set.toList <$> get >>=)
       . (.) (lift . modify . flip (//)) . (++) . fst <*>)
    . uncurry . (. (>>) . (lift . modify . flip (//))) . flip (.)
    . ((>>) <$> (modify . Set.union . Set.fromAscList) <*>)
    . (. flip filter . concatMap
       (flip map (delete (0, 0) $ range ((-1, -1), (1, 1))) . zipPos (+)))
    . (.) (flip Set.notMember <$> get >>=) . (.))
   <$> indices <$> get >>= (`evalStateT` Set.empty))
  . listArray ((0, 0), (9, 9))
  . concat
