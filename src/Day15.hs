import qualified Data.Set as Set
import Bits

type Pos = (Int, Int)
type Grid = Array Pos Int
type DState = State (Set.Set Pos, Set.Set (Int, Pos))

main :: IO ()
main = do
  input <- getContents
  let ls = map (map digitToInt) $ lines input
      gridSize@(w, h) = (length $ head ls, length ls)
      grid = listArray ((0, 0), (w-1, h-1)) $ concat ls :: Grid
      getRisk pos = succ $ (`mod` 9) $ pred $
                    (grid ! zipPos mod pos gridSize) +
                    (uncurry (+) $ zipPos div pos gridSize)
      solve sf = print $ dijkstras getRisk ((0, 0), join mapP (pred . (sf*)) gridSize)
  forM_ [1, 5] solve
  where dijkstras :: (Pos -> Int) -> (Pos, Pos) -> Int
        dijkstras getRisk bounds@(start, end) = ret
          where ret = evalState stepAll (Set.empty, Set.fromList [(0, start)])
                stepAll :: DState Int
                stepAll = fmap (head . concat) $ sequence $ repeat $ stepOnce
                queuePop :: DState (Int, Pos)
                queuePop = do
                  (seen, queue) <- get
                  let (ret, rest) = Set.deleteFindMin queue
                  put (seen, rest)
                  return ret
                stepOnce :: DState [Int]
                stepOnce = do
                  (nextRisk, nextPos) <- queuePop
                  (seen, queue) <- get
                  if nextPos `Set.member` seen
                    then return empty
                    else
                    if nextPos == end
                    then return [nextRisk]
                    else do
                      put (nextPos `Set.insert` seen,
                           Set.union queue
                           $ Set.fromList
                           [(nextRisk + getRisk p, p)
                           | p <- neighbours nextPos,
                             p `Set.notMember` seen,
                             inRange bounds p])
                      return empty

        neighbours :: Pos -> [Pos]
        neighbours pos = map (zipPos (+) pos) [(-1, 0), (1, 0), (0, -1), (0, 1)]
