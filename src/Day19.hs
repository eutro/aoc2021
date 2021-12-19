import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Pos = (Int, Int, Int)
type Scan = Set.Set Pos
type Dists = Map.Map Int Int
type ScanWithDists = (Scan, Dists)
type CompleteProbe = (Pos, ScanWithDists)
type ProcessedScan = ([Scan], Dists)

allUps (x,y,z) =
  [(x,y,z),
   (x,z,-y),
   (x,-y,-z),
   (x,-z,y)]

allOrientations (x,y,z) =
  concatMap allUps
  [(x,y,z),
   (-x,z,y),
   (z,-y,x),
   (-z,x,-y),
   (y,x,-z),
   (-y,-z,x)]

(x1,y1,z1) .- (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)
(x1,y1,z1) .+ (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

manhattan a b = abs x + abs y + abs z
  where (x,y,z) = a .- b

main :: IO ()
main = do
  scans <- map (map ((read :: String -> Pos) . ("("++) . (++")")) . tail . lines)
           . splitOn "\n\n"
           <$> getContents
  let (probes, beacons) = resolveProbes scans
  print $ Set.size beacons
  print $ maximum $ manhattan <$> probes <*> probes
  where resolveProbes :: [[Pos]] -> ([Pos], Scan)
        resolveProbes scans = mapRight (Set.unions . map fst) $ unzip $ concat batches
          where batches :: [[(Pos, ScanWithDists)]]
                -- place probes in batches because the concatenation is too eager
                batches =
                  ([((0,0,0), computeDists $ Set.fromList (head scans))]:)
                  $ takeWhile (not . null)
                  $ evalState
                  (traverse placeNextBatch batches)
                  (map processScan (tail scans))

        computeDists :: Scan -> ScanWithDists
        computeDists scan = (scan, freqs)
          where points = Set.toList scan
                freqs = Map.map (`div` 2)
                        $ Map.delete 0
                        $ frequencies
                        $ manhattan <$> points <*> points

        processScan :: [Pos] -> ProcessedScan
        processScan raw = (sets, snd $ computeDists unrotated)
          where sets@(unrotated:_) = map Set.fromList $ transpose $ map allOrientations raw

        placeNextBatch :: [(Pos, ScanWithDists)] -> State [ProcessedScan] [(Pos, ScanWithDists)]
        placeNextBatch batch = concat <$> traverse placeProbesAgainst (map snd batch)

        placeProbesAgainst :: ScanWithDists -> State [ProcessedScan] [(Pos, ScanWithDists)]
        placeProbesAgainst probe = state
          $ partitionEithers . map (fromMaybe . Right <*> fmap Left . tryMatch probe)

        tryMatch :: ScanWithDists -> ProcessedScan -> Maybe (Pos, ScanWithDists)
        tryMatch (probeA, distsA) (probeBs, distsB) = listToMaybe $ do
          let commonDists = Map.intersectionWith min distsA distsB
          -- the scans can only match if they share enough distances;
          -- the converse is not guaranteed, although it is actually true in practice.
          -- this guard is also invariant under rotations, saving a lot of time
          guard (sum commonDists >= 66) -- 12 choose 2
          probeB <- probeBs
          let (offset, offsetC) = maximumBy (compare `on` snd)
                                  $ Map.toList
                                  $ frequencies
                                  $ (.-) <$> Set.toList probeA <*> Set.toList probeB
          guard (offsetC >= 12)
          let mapped = Set.mapMonotonic (.+offset) probeB
          return (offset, (mapped, distsB))
