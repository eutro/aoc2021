import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Pos = (Int, Int)
type Grid = Array Pos Bool
type Amphs = Map.Map Int (Set.Set Pos)

main :: IO ()
main = do
  input <- lines <$> getContents :: IO [[Char]]
  let amphs = parseGrid input
      p1 = dijkstras target1 amphs
  -- printPath True $ reverse $ snd p1
  print $ fst $ p1
  let amphs' = parseGrid ((take 3 input) ++
                          ["  #D#C#B#A#",
                           "  #D#B#A#C#"] ++
                          (drop 3 input))
      p2 = dijkstras target2 amphs'
  -- printPath False $ reverse $ snd p2
  print $ fst $ p2
  where printPath :: Bool -> [Amphs] -> IO ()
        printPath p1 path = do
          forM_ path $ \ state -> do
            let height = if p1 then 5 else 7
                arr = listArray ((1,1), (13, height))
                      $ concat
                      $ transpose
                      $ concat [["#############",
                                 "#...........#",
                                 "###.#.#.#.###"],
                                replicate (height - 4)
                                "  #.#.#.#.#  ",
                                ["  #########  "]]
                arr' = arr // [(pos, toEnum $ fromEnum 'A' + idx) |
                               (idx, poss) <- Map.toList state,
                               pos <- Set.toList poss]
                grid = transpose
                       $ map (take height)
                       $ takeWhile (not . null)
                       $ iterate (drop height) (elems arr')
            putStrLn $ unlines grid

        parseGrid input = amphs
          where charGrid = listArray ((1, 1), (length input, length $ head input))
                           $ concat $ (take 3 input) ++ (map (++"  ") $ drop 3 input)
                amphs = Map.fromListWith Set.union
                  $ [(fromEnum char - fromEnum 'A', Set.fromList [swap pos])
                    | (pos, char) <- assocs charGrid,
                      char `elem` "ABCD"]

        dijkstras :: Amphs -> Amphs -> (Int, [Amphs])
        dijkstras target amphs = run'
          where run = evalStateT (dijkstras' target) Set.empty
                run' = evalState run $ Set.fromList [(0, [amphs])]
        dijkstras' :: Amphs ->
                      (StateT (Set.Set Amphs))
                      (State (Set.Set (Int, [Amphs])))
                      (Int, [Amphs])
        dijkstras' target = do
          seen <- get
          (energy, path) <- lift $ state Set.deleteFindMin
          let amphs = head path
          if amphs `Set.member` seen
            then dijkstras' target
            else do
            modify $ Set.insert amphs
            let states = map (mapRight (:path))
                         $ filter ((`Set.notMember` seen) . snd)
                         $ nextStates target energy amphs
                output = listToMaybe $ filter ((==target) . head . snd) states
            case output of
              Just ret -> return ret
              _ -> do
                lift $ modify $ Set.union $ Set.fromList $ states
                dijkstras' target

        nextStates :: Amphs -> Int -> Amphs -> [(Int, Amphs)]
        nextStates target energy amphs = do
          (idx, set) <- Map.toList amphs
          pos <- Set.toList set
          let set' = pos `Set.delete` set
              tileClear tile = all (Set.notMember tile) amphs
              pathClear path = all tileClear path
              yieldPath path =
                [(energy + energies!!idx * length path,
                  Map.insert idx (Set.insert (last path) set') amphs)]
          if pos `Set.member` hallway
            then do
            let amphTarget = target Map.! idx
            guard
              $ not
              $ any (not . Set.null . Set.intersection amphTarget)
              $ Map.delete idx amphs
            let firstTile@(x,_) = Set.findMin amphTarget
                fstPart = tail $
                          (if pos <= (x,2)
                           then range
                           else reverse . range . swap)
                          (pos, (x,2))
                sndPart = Set.toList $ amphTarget `Set.difference` set'
            guard (pathClear fstPart)
            yieldPath (fstPart ++ sndPart)
            else do
            let (x,y) = pos
                fstPart = tail $ reverse (range ((x,3), pos))
            guard (pathClear fstPart)
            targetTile <- Set.toList hallwayNoDoors
            let sndPart = (if (x, 2) < targetTile
                           then range
                           else reverse . range . swap)
                          ((x, 2), targetTile)
            guard (pathClear sndPart)
            yieldPath (fstPart ++ sndPart)

        energies = [1,10,100,1000]
        hallway = Set.fromList (range ((2,2), (12,2)))
        hallwayNoDoors = Set.difference hallway
                         $ Set.fromList [(4,2),(6,2),(8,2),(10,2)]
        target1 = Map.fromList [(0, Set.fromList [(4,3),(4,4)]),
                                (1, Set.fromList [(6,3),(6,4)]),
                                (2, Set.fromList [(8,3),(8,4)]),
                                (3, Set.fromList [(10,3),(10,4)])]
        target2 = Map.fromList [(0, Set.fromList $ range ((4,3),(4,6))),
                                (1, Set.fromList $ range ((6,3),(6,6))),
                                (2, Set.fromList $ range ((8,3),(8,6))),
                                (3, Set.fromList $ range ((10,3),(10,6)))]
