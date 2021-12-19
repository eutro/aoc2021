import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Array
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

type Pos = (Int, Int, Int)
type Scan = Set.Set Pos
type ScanWithDists = (Scan, Map.Map Pos Int)
type CompleteProbe = (Pos, ScanWithDists)
type ProcessedScan = [ScanWithDists]

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
          -- these are maps instead of sets out of principle;
          -- but in practice no distance appears more than once
          where points = Set.toList scan
                freqs = Map.delete (0,0,0) $ frequencies $ (.-) <$> points <*> points

        processScan :: [Pos] -> ProcessedScan
        processScan raw = map (computeDists . Set.fromList) $ transpose $ map allOrientations raw

        placeNextBatch :: [(Pos, ScanWithDists)] -> State [ProcessedScan] [(Pos, ScanWithDists)]
        placeNextBatch batch = concat <$> traverse placeProbesAgainst (map snd batch)

        placeProbesAgainst :: ScanWithDists -> State [ProcessedScan] [(Pos, ScanWithDists)]
        placeProbesAgainst probe = state
          $ partitionEithers . map
          (fromMaybe . Right <*> fmap Left . listToMaybe . mapMaybe (tryMatch probe))

        tryMatch :: ScanWithDists -> ScanWithDists -> Maybe (Pos, ScanWithDists)
        tryMatch (probeA, distsA) (probeB, distsB) = listToMaybe $ do
          let commonDists = Map.intersectionWith min distsA distsB
          -- the scans can only match if they share enough distances;
          -- the converse is not guaranteed, although it is true in practice
          guard (sum commonDists >= 66) -- 12 choose 2
          a <- Set.toList probeA
          b <- Set.toList probeB
          let mapped = Set.mapMonotonic ((.+a) . (.-b)) probeB
              hits = Set.intersection probeA mapped
          guard (Set.size hits >= 12)
          return ((a.-b), (mapped, distsB))
