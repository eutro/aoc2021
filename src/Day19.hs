import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Pos = (Int, Int, Int)
type Scan = [Pos]
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
  where resolveProbes :: [[Pos]] -> ([Pos], Set.Set Pos)
        resolveProbes scans = mapRight (Set.fromList . concatMap fst) $ unzip $ probes
          where probes :: [(Pos, ScanWithDists)]
                probes =
                  (((0,0,0), computeDists $ head scans):)
                  $ concat
                  $ catMaybes
                  $ takeWhile isJust
                  $ evalState (mapM placeProbesAgainst (map snd probes))
                  $ map processScan (tail scans)

        computeDists :: Scan -> ScanWithDists
        computeDists scan = (scan, freqs)
          where freqs = Map.map (`div` 2)
                        $ Map.delete 0
                        $ frequencies
                        $ manhattan <$> scan <*> scan

        processScan :: [Pos] -> ProcessedScan
        processScan raw = (sets, snd $ computeDists unrotated)
          where sets@(unrotated:_) = transpose $ map allOrientations raw

        placeProbesAgainst :: ScanWithDists -> State [ProcessedScan] (Maybe [(Pos, ScanWithDists)])
        placeProbesAgainst probe = mapStateT (maybe (Identity (Nothing, [])) Identity) $ do
          get >>= guard . not . null
          pure <$> state (partitionEithers . map (fromMaybe . Right <*> fmap Left . tryMatch probe))

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
                                  $ (.-) <$> probeA <*> probeB
          guard (offsetC >= 12)
          let mapped = map (.+offset) probeB
          return (offset, (mapped, distsB))
