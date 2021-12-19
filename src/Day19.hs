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
  let probes :: [Pos]
      beacons :: Set.Set Pos
      (probes, beacons) = mapRight Set.unions $ unzip $ concat finds
        where finds = map fst
                $ takeWhile (not . null . fst)
                $ scanl stepFold ([((0,0,0), Set.fromList (head scans))],
                                  (map (map Set.fromList . transpose . map allOrientations)
                                   $ tail scans))
                finds
              stepFold :: ([(Pos, Set.Set Pos)], [[Set.Set Pos]]) ->
                          [(Pos, Set.Set Pos)] ->
                          ([(Pos, Set.Set Pos)], [[Set.Set Pos]])
              stepFold (_, remaining) probes =
                let results = scanl
                      (\ (_, remaining) set -> matchesWith set remaining)
                      ([], remaining)
                      $ map snd probes
                in (concatMap fst results, snd $ last results)
  print $ Set.size beacons
  print $ maximum $ manhattan <$> probes <*> probes
  return ()
  where doMatch :: Set.Set Pos -> Set.Set Pos -> Maybe (Pos, Set.Set Pos)
        doMatch probeA probeB = fmap fst $ uncons $ do
          a <- Set.toList probeA
          b <- Set.toList probeB
          let mapped = Set.mapMonotonic ((.+a) . (.-b)) probeB
              hits = Set.intersection probeA mapped
          guard (Set.size hits >= 12)
          trace (show (a.-b)) $ return ((a.-b), mapped)

        tryMatch :: Set.Set Pos -> [Set.Set Pos] -> Maybe (Pos, Set.Set Pos)
        tryMatch probeA probeBs =
          foldl (<|>) Nothing
          $ map (doMatch probeA) probeBs

        matchesWith :: Set.Set Pos -> [[Set.Set Pos]] -> ([(Pos, Set.Set Pos)], [[Set.Set Pos]])
        matchesWith probe restProbes =
          partitionEithers [maybe (Right rp) Left $ tryMatch probe rp | rp <- restProbes]
