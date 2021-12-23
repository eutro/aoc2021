import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Rooms = Array Int [Int]
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
                  $ transpose
                  $ map (mapMaybe (`Map.lookup` (Map.fromList $ zip "ABCD" [1,2,3,4])))
                  $ take 2
                  $ drop 2
                  $ lines str

        augment :: Amphs -> Amphs
        augment amphs =
          Amphs
          (listArray (1,4)
           [ [a,b,c,d]
           | ([a, d], [b, c]) <- zip
             (elems $ rooms amphs)
             [[4,4], [3,2], [2,1], [1,3]] -- given in puzzle
           ])
          (hallway amphs)

        dijkstras :: Amphs -> Int
        dijkstras amphs = fst run'
          where run = evalStateT loop Set.empty
                run' = evalState run $ Set.fromList [(0, [(0, amphs)])]
                roomSize = length (rooms amphs ! 1)

                loop :: (StateT (Set.Set Amphs))
                        (State (Set.Set (Int, [(Int, Amphs)])))
                        (Int, [(Int, Amphs)])
                loop = do
                  seen <- get
                  (energy, path) <- lift $ state Set.deleteFindMin
                  let (_, amphs) = head path
                  if amphs `Set.member` seen
                    then loop
                    else if (all isNothing $ hallway amphs) &&
                            (all
                             (\ (roomIdx, inside) -> all (==roomIdx) inside)
                             (assocs $ rooms amphs))
                    then return (energy, path)
                    else do
                    modify $ Set.insert amphs
                    let states = map (\ nb@(w,_) -> (energy + w, (nb:path)))
                          $ filter ((`Set.notMember` seen) . snd)
                          $ nextStates roomSize amphs
                    lift $ modify $ Set.union $ Set.fromList $ states
                    loop

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
                  let targetRoom = (rooms amphs ! toMove)
                  guard (all (==toMove) targetRoom)
                  guard (all
                         (isNothing . (hallway amphs!))
                         (pathBetween hallwayIdx (2*toMove)))
                  return -- move into destination room
                    ((energies ! toMove) *
                     ((abs $ 2*toMove - hallwayIdx) + -- along hallway
                      (roomSize - length targetRoom)), -- into room
                     Amphs
                     (rooms amphs // [(toMove, toMove : targetRoom)])
                     (hallway amphs // [(hallwayIdx, Nothing)]))
                movedFromRoom = do
                  (roomIdx, inRoom) <- assocs $ rooms amphs
                  -- don't bother popping from a happy room,
                  -- also guarantees at least one in the list
                  guard (any (/=roomIdx) inRoom)
                  let (toMove:restInRoom) = inRoom
                      open = hallwayOpen (hallway amphs) (2*roomIdx)
                      targetRoom = (rooms amphs ! toMove)
                  if (2*toMove) `elem` open &&
                     (all (==roomIdx) targetRoom)
                    then do
                    return -- move direclty into target room
                      ((energies ! toMove) *
                       ((roomSize - length restInRoom) + -- out of room
                        (abs (2*roomIdx - 2*toMove)) + -- along hallway
                        (roomSize - length targetRoom)), -- into room
                       Amphs
                       (rooms amphs //
                        [(roomIdx, restInRoom),
                         (toMove, toMove : targetRoom)])
                       (hallway amphs))
                    else do
                    hallwaySlot <- open
                    guard (hallwaySlot `notElem` [2,4..8])
                    return -- wait in hallway
                      ((energies ! toMove) *
                       ((roomSize - length restInRoom) + -- out of room
                        (abs $ 2*roomIdx - hallwaySlot)), -- along hallway
                       Amphs
                       (rooms amphs // [(roomIdx, restInRoom)])
                       (hallway amphs // [(hallwaySlot, Just toMove)]))
