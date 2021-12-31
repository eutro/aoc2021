import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array.MArray
import Bits

data Room = Room Int [Int]
  deriving (Ord, Eq, Show)

type Rooms = Array Int Room
type Hallway = Array Int (Maybe Int)

data Amphs = Amphs
  { rooms :: Rooms
  , hallway :: Hallway }
  deriving (Ord, Eq, Show)

hallwayLen = 10

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ dijkstras input
  print $ dijkstras $ augment input
  return ()
  where parseInput :: String -> Amphs
        parseInput str = Amphs amphRooms amphHallway
          where amphHallway = listArray (0,hallwayLen) $ replicate 11 Nothing
                amphRooms =
                  listArray (1,4)
                  $ map (Room 0)
                  $ transpose
                  $ map (mapMaybe (`Map.lookup` (Map.fromList $ zip "ABCD" [1,2,3,4])))
                  $ take 2
                  $ drop 2
                  $ lines str

        normaliseRooms :: Amphs -> Amphs
        normaliseRooms amphs =
          Amphs
          (listArray (1,4) $ map mapRoom $ assocs $ rooms amphs)
          (hallway amphs)
          where mapRoom (id, Room happy unhappy) =
                  Room (happy + length h) (reverse uh)
                  where (h, uh) = span (==id) $ reverse unhappy

        augment :: Amphs -> Amphs
        augment amphs =
          Amphs
          (listArray (1,4)
           [Room 0 [a,b,c,d]
           | (Room 0 [a, d], [b, c]) <- zip
             (elems $ rooms amphs)
             [[4,4], [3,2], [2,1], [1,3]] -- given in puzzle
           ])
          (hallway amphs)

        dijkstras :: Amphs -> Int
        dijkstras amphs = run amphs
          where roomSize = roomFill $ rooms amphs ! 1

                isFull (Room fill []) = fill == roomSize
                isFull _ = False

                run :: Amphs -> Int
                run start = runST $ do
                  seenH <- newHash (2^18) :: ST s (Hashtable s Amphs ())
                  heap <- newArray (1, 2^16) undefined :: ST s (STArray s Int (Int, Amphs))
                  -- heapIndex <- newHash (2^16) :: ST s (Hashtable s Amphs Int)
                  heapSize <- newSTRef 0 :: ST s (STRef s Int)
                  let heapPop = do
                        size <- readSTRef heapSize
                        head <- readArray heap 1
                        modifySTRef' heapSize pred
                        when (size > 1) $ do
                          readArray heap size >>= writeArray heap 1
                          bubbleDown 1
                        return head
                      heapSwap = \ a b -> do
                        ap@(_, av) <- readArray heap a
                        bp@(_, bv) <- readArray heap b
                        writeArray heap a bp
                        writeArray heap b ap
                        -- hashPut heapIndex av b
                        -- hashPut heapIndex bv a
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
                      heapPush el@(k, v) = do
                        -- shouldAdd <- maybe True (k<) <$> hashRef heapIndex v
                        -- when shouldAdd $ do
                          modifySTRef' heapSize succ
                          idx <- readSTRef heapSize
                          writeArray heap idx el
                          -- hashPut heapIndex v idx
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
                    (energy, amphs) <- heapPop
                    hasSeen <- hashContains seenH amphs
                    if hasSeen
                      then loop
                      else if all isFull $ elems $ rooms amphs
                      then return energy
                      else do
                      hashPut seenH amphs ()
                      let nbs = nextStates roomSize amphs
                      nbs' <- filterM (fmap not . (hashContains seenH) . snd) nbs
                      let states = map (mapLeft (energy+)) nbs'
                      mapM_ heapPush states
                      loop

        isHappy (Room _ []) = True
        isHappy _ = False

        roomFill (Room happy unhappy) = happy + length unhappy

        hallwayOpen :: Hallway -> Int -> [Int]
        hallwayOpen hallway from = left ++ right
          where left = takeWhile (isNothing . (hallway!)) $ reverse [0..pred from]
                right = takeWhile (isNothing . (hallway!)) [succ from..hallwayLen]

        pathBetween start end
          | start < end = [succ start..end]
          | otherwise = [end..pred start]

        energies = listArray (1,4) [1,10,100,1000]

        nextStates :: Int -> Amphs -> [(Int, Amphs)]
        nextStates roomSize amphs = movedFromRoom ++ movedFromHallway
          where movedFromHallway = do
                  (hallwayIdx, maybeAmph) <- assocs $ hallway amphs
                  toMove <- maybeToList maybeAmph
                  Room targetRoom [] <- return $ rooms amphs ! toMove
                  guard (all
                         (isNothing . (hallway amphs!))
                         (pathBetween hallwayIdx (2*toMove)))
                  return -- move into destination room
                    ((energies ! toMove) *
                     ((abs $ 2*toMove - hallwayIdx) + -- along hallway
                      (roomSize - targetRoom)), -- into room
                     Amphs
                     (rooms amphs // [(toMove, Room (succ targetRoom) [])])
                     (hallway amphs // [(hallwayIdx, Nothing)]))
                movedFromRoom = do
                  (roomIdx, Room happy (toMove:restInRoom)) <- assocs $ rooms amphs
                  -- don't bother popping from a happy room,
                  let open = hallwayOpen (hallway amphs) (2*roomIdx)
                      targetRoom = (rooms amphs ! toMove)
                      oldRoom = Room happy restInRoom
                  if isHappy targetRoom && (2*toMove) `elem` open
                    then do
                    let Room inTarget [] = targetRoom
                    return -- move direclty into target room
                      ((energies ! toMove) *
                       ((roomSize - roomFill oldRoom) + -- out of room
                        (abs (2*roomIdx - 2*toMove)) + -- along hallway
                        (roomSize - inTarget)), -- into room
                       Amphs
                       (rooms amphs //
                        [(roomIdx, oldRoom),
                         (toMove, Room (succ inTarget) [])])
                       (hallway amphs))
                    else do
                    hallwaySlot <- open
                    guard (hallwaySlot `notElem` [2,4..8])
                    return -- wait in hallway
                      ((energies ! toMove) *
                       ((roomSize - roomFill oldRoom) + -- out of room
                        (abs $ 2*roomIdx - hallwaySlot)), -- along hallway
                       Amphs
                       (rooms amphs // [(roomIdx, oldRoom)])
                       (hallway amphs // [(hallwaySlot, Just toMove)]))

class Hash a where hash :: a -> Int

instance Hash Int where hash = id
instance Hash a => Hash [a] where hash l = foldl ((+) . (*31)) 0 $ map hash l
instance (Hash a, Hash b) => Hash (a, b) where hash (x, y) = 27 * hash x + hash y
instance (Hash a, Hash b) => Hash (Array a b) where hash arr = hash ((bounds arr), elems arr)
instance Hash Amphs where hash (Amphs a b) = hash (a, b)
instance Hash Room where hash (Room x y) = hash (x, y)

instance Hash a => Hash (Maybe a) where
  hash (Just x) = 2 * hash x + 1
  hash Nothing = 0

hashSpread :: Hash k => k -> Int
hashSpread key = h `xor` (h `shiftR` 16)
  where h = hash key

data Hashtable s k v = Hashtable { hashSize :: Int, buckets :: STArray s Int (Map.Map k v) }
newHash :: Int -> ST s (Hashtable s k v)
newHash size = Hashtable size <$> newArray (0, size) Map.empty

hashSeq :: Hashtable s k v -> ST s [(k, v)]
hashSeq (Hashtable _ buckets) = concatMap Map.toList <$> getElems buckets

hashPut :: (Hash k, Eq k, Ord k) => Hashtable s k v -> k -> v -> ST s ()
hashPut (Hashtable size buckets) key val = do
  let bucketNo = hash key `mod` size
  bucket <- readArray buckets bucketNo
  let newBucket = Map.insert key val bucket
  writeArray buckets bucketNo newBucket
  return ()

hashRef :: (Hash k, Eq k, Ord k) => Hashtable s k v -> k -> ST s (Maybe v)
hashRef (Hashtable size buckets) key = do
  let bucketNo = hash key `mod` size
  bucket <- readArray buckets bucketNo
  return $ Map.lookup key bucket

hashContains :: (Hash k, Eq k, Ord k) => Hashtable s k v -> k -> ST s Bool
hashContains hash key = isJust <$> hashRef hash key
