import qualified Data.Set as Set
import qualified Data.Map as Map
import Bits hiding (gridBounds)

type Pos = (Int, Int)
type Grid = Array Pos Int

main :: IO ()
main = do
  input <- getContents
  let grid = listArray gridBounds $ concat $ map (map digitToInt) $ lines input
      counts = evalState (sequence (repeat tick)) grid
      phase = succ $ length $ takeWhile (/=100) $ counts
      prefixSums = listArray (0, phase) $ scanl (+) 0 counts
      sumBetween 0 x
        | x < phase = prefixSums!x
        | otherwise = (prefixSums!phase) + ((x - phase) `div` 10) * 100
      sumBetween a b = sumBetween 0 b - sumBetween 0 (pred a)
  print $ sumBetween 0 100
  print $ phase
  where neighbours = flip map (delete (0, 0) $ range ((-1, -1), (1, 1))) . zipPos (+)
        gridBounds :: (Pos, Pos)
        gridBounds = ((0, 0), (9, 9))
        tick :: State Grid Int
        tick = tick1 <$> indices <$> get >>= (`evalStateT` Set.empty)
        tick1 :: [Pos] -> (StateT (Set.Set Pos)) (State Grid) Int
        tick1 idcs = do
          grid <- lift get
          let (assocs, flashes') = unzip $ do
                (idx, v) <- Map.toList $ frequencies idcs
                guard (inRange gridBounds idx)
                let newv = grid!idx + v
                return ((idx, newv), if newv > 9 then Just idx else Nothing)
                --                                         ^^^
                --           guaranteed to be yielded in ascending order
                --           because it comes from a map
              flashes = catMaybes flashes' -- guaranteed to be ascending
          if null flashes
            then do
            seen <- get
            lift $ modify $ (//(assocs ++ map (flip (,) 0) (Set.toList seen)))
            Set.size <$> get
            else do
            modify $ Set.union $ Set.fromAscList flashes
            --                   ^^^^^^^^^^^^^^^ safety: flashes is ascending
            lift $ modify (//assocs)
            seen <- get
            tick1 $ filter (`Set.notMember` seen) $ concatMap neighbours flashes
            
