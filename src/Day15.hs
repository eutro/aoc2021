import qualified Data.Set as Set
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array.MArray
import Bits

type Pos = (Int, Int)
type Grid = Array Pos Int

main :: IO ()
main1 = map (map digitToInt) <$> lines <$> getContents >>=
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

main = do
  input <- getContents
  let ls = map (map digitToInt) $ lines input
      gridSize@(w, h) = (length $ head ls, length ls)
      grid = listArray ((0, 0), (w-1, h-1)) $ concat ls :: Grid
      getRisk pos = (`mod1` 9) $
                    (grid ! zipPos mod pos gridSize) +
                    (uncurry (+) $ zipPos div pos gridSize)
      solve sf = print $ dijkstras getRisk ((0, 0), join mapP (pred . (sf*)) gridSize)
  forM_ [1, 5] solve
  where dijkstras :: (Pos -> Int) -> (Pos, Pos) -> Int
        dijkstras getRisk bounds@(start, end) = runST $ do
          -- array for seen set provides fast and constant time access;
          -- unboxing also improves speed by ~50%
          seen <- newArray bounds False :: ST s (STUArray s Pos Bool)
          -- simple binary heap, much quicker than persistent-set-based queues;
          -- size was big enough in testing
          heap <- newArray (1, 2^10) undefined :: ST s (STArray s Int (Int, Pos))
          heapSize <- newSTRef 0 :: ST s (STRef s Int)
          let -- traceHeap = \ x -> do
              --   size <- readSTRef heapSize
              --   heapSlice <- mapM (readArray heap) [1..size]
              --   trace (show heapSlice) x
              heapPop = do
                size <- readSTRef heapSize
                -- when (size <= 0) $ fail "heapPop - empty heap"
                head <- readArray heap 1
                modifySTRef heapSize pred
                when (size > 1) $ do
                  readArray heap size >>= writeArray heap 1
                  bubbleDown 1
                return head
              heapSwap = \ a b -> do
                tmp <- readArray heap a
                readArray heap b >>= writeArray heap a
                writeArray heap b tmp
                return ()
              bubbleDown i = do
                size <- readSTRef heapSize
                let findTop = \ new old ->
                      if new > size then return old
                      else do
                        swap <- ((<) `on` fst)
                                <$> readArray heap new
                                <*> readArray heap old
                        return $ if swap then new else old
                    left = 2*i
                    right = 2*i + 1
                top <- findTop left i >>= findTop right
                when (top /= i) $ do
                  heapSwap i top
                  bubbleDown top
              heapPush el = do
                modifySTRef heapSize succ
                idx <- readSTRef heapSize
                writeArray heap idx el
                bubbleUp idx
                return ()
              bubbleUp i
                | i == 1 = return ()
                | otherwise = do
                let parent = div i 2
                swap <- ((<) `on` fst)
                        <$> readArray heap i
                        <*> readArray heap parent
                when swap $ do
                  heapSwap i parent
                  bubbleUp parent
          heapPush (0, start)
          fix $ \ loop -> do
            (risk, pos) <- heapPop
            nbs <- flip filterM (neighbours pos) $ \ nb ->
              if inRange bounds nb
              then not <$> readArray seen nb
              else return False
            (>>= either return (const loop))
              $ fmap sequence
              $ forM nbs $ \ nb ->
              let nbr = risk + getRisk nb
              in if nb == end
              then return $ Left nbr
              else Right <$> do
                heapPush (nbr, nb)
                writeArray seen nb True
                return ()

        neighbours :: Pos -> [Pos]
        neighbours pos = map (addPos pos) [(-1, 0), (1, 0), (0, -1), (0, 1)]
