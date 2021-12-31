import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Array.MArray
import Bits

data Room = Happy Int | Unhappy [Int]
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
                  $ map Unhappy
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
          where mapRoom (id, Unhappy room)
                  | all (==id) room = Happy (length room)
                mapRoom (_, raw) = raw

        augment :: Amphs -> Amphs
        augment amphs =
          Amphs
          (listArray (1,4)
           [Unhappy [a,b,c,d]
           | (Unhappy [a, d], [b, c]) <- zip
             (elems $ rooms amphs)
             [[4,4], [3,2], [2,1], [1,3]] -- given in puzzle
           ])
          (hallway amphs)

        dijkstras :: Amphs -> Int
        dijkstras amphs = run amphs
          where roomSize = findSize $ elems $ rooms amphs

                findSize (Unhappy l : _) = length l
                findSize (_ : r) = findSize r

                isFull (Happy fill) = fill == roomSize
                isFull _ = False

                run :: Amphs -> Int
                run start = runST $ do
                  seenR <- newSTRef Set.empty
                           :: ST s (STRef s (Set.Set Amphs))
                  heap <- newSTRef $ Set.singleton (0, start)
                          :: ST s (STRef s (Set.Set (Int, Amphs)))
                  fix $ \ loop -> do
                    seen <- readSTRef seenR
                    ((energy, amphs), rem) <- Set.deleteFindMin <$> readSTRef heap
                    writeSTRef heap rem
                    if amphs `Set.member` seen
                      then loop
                      else if all isFull $ elems $ rooms amphs
                      then return energy
                      else do
                      modifySTRef' seenR $ Set.insert amphs
                      let states = map (mapLeft (energy+))
                            $ filter ((`Set.notMember` seen) . snd)
                            $ nextStates roomSize amphs
                      modifySTRef' heap $ Set.union $ Set.fromList $ states
                      loop

        isHappy (Happy _) = True
        isHappy _ = False

        roomFromList idx ls
          | all (==idx) ls = Happy $ length ls
          | otherwise = Unhappy ls

        roomFill (Happy fill) = fill
        roomFill (Unhappy l) = length l

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
                  Happy targetRoom <- return $ rooms amphs ! toMove
                  guard (all
                         (isNothing . (hallway amphs!))
                         (pathBetween hallwayIdx (2*toMove)))
                  return -- move into destination room
                    ((energies ! toMove) *
                     ((abs $ 2*toMove - hallwayIdx) + -- along hallway
                      (roomSize - targetRoom)), -- into room
                     Amphs
                     (rooms amphs // [(toMove, Happy $ succ targetRoom)])
                     (hallway amphs // [(hallwayIdx, Nothing)]))
                movedFromRoom = do
                  (roomIdx, Unhappy inRoom) <- assocs $ rooms amphs
                  -- don't bother popping from a happy room,
                  let (toMove:restInRoom) = inRoom
                      open = hallwayOpen (hallway amphs) (2*roomIdx)
                      targetRoom = (rooms amphs ! toMove)
                      oldRoom = roomFromList roomIdx restInRoom
                  if isHappy targetRoom && (2*toMove) `elem` open
                    then do
                    let Happy inTarget = targetRoom
                    return -- move direclty into target room
                      ((energies ! toMove) *
                       ((roomSize - roomFill oldRoom) + -- out of room
                        (abs (2*roomIdx - 2*toMove)) + -- along hallway
                        (roomSize - inTarget)), -- into room
                       Amphs
                       (rooms amphs //
                        [(roomIdx, oldRoom),
                         (toMove, Happy $ succ inTarget)])
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
