{-# LANGUAGE MultiParamTypeClasses, KindSignatures, FlexibleInstances #-}

module STUtil where

import Data.Array.MArray
import Data.Array.ST
import Data.Function
import Data.STRef
import Control.Monad
import Control.Monad.ST
import Debug.Trace

class Monad m => MVector a e m where
  vecLen :: a e -> m Int
  vecResize :: a e -> Int -> e -> m ()
  vecRead :: a e -> Int -> m e
  vecWrite :: a e -> Int -> e -> m ()
  vecPush :: a e -> e -> m ()
  vecSwap :: a e -> Int -> Int -> m ()
  vecSwap vec a b = do
      oldA <- vecRead vec a
      oldB <- vecRead vec b
      vecWrite vec a oldB
      vecWrite vec b oldA
  newVec :: m (a e)

data STFixedVector s e = STFixedVector
  { lenCell :: STRef s Int
  , bufCell :: STRef s (STArray s Int e)}

stVecResizeBuf vec size = do
  oldBuf <- readSTRef $ bufCell vec
  oldEls <- getElems oldBuf
  newBuf <- newListArray (1, size) oldEls
  writeSTRef (bufCell vec) newBuf
  return newBuf

instance MVector (STFixedVector s) e (ST s) where
  vecLen vec = readSTRef $ lenCell vec
  vecResize vec size el = do
    oldLen <- readSTRef $ lenCell vec
    writeSTRef (lenCell vec) size
    buf <- readSTRef $ bufCell vec
    (_, bufSz) <- getBounds buf
    buf' <-
      if bufSz < size
      then stVecResizeBuf vec size
      else return buf
    if size > oldLen
      then forM_ [succ oldLen..size] $ \ idx ->
      writeArray buf' idx el
      else forM_ [succ size..oldLen] $ \ idx ->
      writeArray buf' idx undefined
  vecRead vec idx = do
    buf <- readSTRef $ bufCell vec
    readArray buf idx
  vecWrite vec idx val = do
    buf <- readSTRef $ bufCell vec
    writeArray buf idx val
  vecPush vec val = do
    len <- readSTRef $ lenCell vec
    buf <- readSTRef $ bufCell vec
    (_, size) <- getBounds buf
    let len' = succ len
    buf' <-
      if len' > size
      then stVecResizeBuf vec (size * 2)
      else return buf
    writeSTRef (lenCell vec) len'
    writeArray buf' len' val
  newVec = do
    let size = 16
    lenCell <- newSTRef 0
    buf <- newArray (1, size) undefined
    bufCell <- newSTRef buf
    return $ STFixedVector lenCell bufCell

class Monad m => MHeap a k v m where
  heapPop :: a k v -> m (k, v)
  heapPush :: a k v -> (k, v) -> m ()
  newHeap :: m (a k v)

data STBinHeap s k v = STBinHeap
  { heapVec :: STFixedVector s (k, v) }

stHeapSwap :: STBinHeap s k v -> Int -> Int -> ST s ()
stHeapSwap heap a b = vecSwap (heapVec heap) a b

stHeapBubbleUp heap i
  | i == 1 = return ()
  | otherwise = do
      let parent = div i 2
      swap <- ((<) `on` fst)
              <$> vecRead (heapVec heap) i
              <*> vecRead (heapVec heap) parent
      when swap $ do
        stHeapSwap heap i parent
        stHeapBubbleUp heap parent

stHeapBubbleDown heap i = do
  let vec = heapVec heap
  size <- vecLen vec
  let findTop = \ new old ->
        if new > size then return old
        else do
          swap <- ((<) `on` fst)
                  <$> vecRead vec new
                  <*> vecRead vec old
          return $ if swap then new else old
      left = 2*i
      right = 2*i + 1
  top <- findTop left i >>= findTop right
  when (top /= i) $ do
    stHeapSwap heap i top
    stHeapBubbleDown heap top

instance Ord k => MHeap (STBinHeap s) k v (ST s) where
  newHeap = STBinHeap <$> newVec
  heapPush heap el = do
    let vec = (heapVec heap)
    vecPush vec el
    len <- vecLen vec
    stHeapBubbleUp heap len
  heapPop heap = do
    let vec = (heapVec heap)
    len <- vecLen vec
    -- when (size <= 0) $ fail "heapPop - empty heap"
    head <- vecRead vec 1
    when (len > 1) $ do
      newHead <- vecRead vec len
      vecWrite vec 1 newHead
      stHeapBubbleDown heap 1
    vecResize vec (pred len) undefined
    return head

-- test = runST $ do
--   heap <- newHeap :: ST s (STBinHeap s Int Int)
--   sequence
--     [do heapPush heap (32, 1)
--         heapPush heap (64, 1)
--         heapPush heap (16, 1)
--         heapPop heap,
--      do heapPush heap (8, 1)
--         heapPush heap (4, 1)
--         heapPop heap,
--      heapPop heap,
--      heapPop heap,
--      heapPop heap]
