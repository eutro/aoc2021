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
  scans <- map (map ((read :: String -> Pos) . ("("++) . (++")"))
                . tail
                . lines)
           . splitOn "\n\n"
           <$> getContents
  let (probes, beacons) = resolveProbes scans
  print $ Set.size beacons
  print $ maximum $ manhattan <$> probes <*> probes
  where resolveProbes :: [[Pos]] -> ([Pos], Set.Set Pos)
        resolveProbes scans = mapRight Set.unions $ unzip $ concat finds
          where finds :: [[(Pos, Set.Set Pos)]]
                finds =
                  ([((0,0,0), Set.fromList (head scans))]:)
                  $ takeWhile (not . null)
                  $ evalState
                  (traverse stepFold finds)
                  (map probeAllOrientations (tail scans))

        probeAllOrientations :: [Pos] -> [Set.Set Pos]
        probeAllOrientations probe = map Set.fromList $ transpose $ map allOrientations probe

        stepFold :: [(Pos, Set.Set Pos)] -> State [[Set.Set Pos]] [(Pos, Set.Set Pos)]
        stepFold probes = concat <$> traverse matchesWith (map snd probes)

        matchesWith :: Set.Set Pos -> State [[Set.Set Pos]] [(Pos, Set.Set Pos)]
        matchesWith probe = state
          $ partitionEithers . map
          (fromMaybe . Right <*> fmap Left . listToMaybe . mapMaybe (doMatch probe))

        doMatch :: Set.Set Pos -> Set.Set Pos -> Maybe (Pos, Set.Set Pos)
        doMatch probeA probeB = listToMaybe $ do
          a <- Set.toList probeA
          b <- Set.toList probeB
          let mapped = Set.mapMonotonic ((.+a) . (.-b)) probeB
              hits = Set.intersection probeA mapped
          guard (Set.size hits >= 12)
          return ((a.-b), mapped)
