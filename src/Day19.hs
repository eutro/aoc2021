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

manhattan a b = let (x,y,z) = a .- b in
                  abs x + abs y + abs z

main :: IO ()
main = do
  probes <- map (map ((read :: String -> Pos) . ("("++) . (++")"))
                 . tail
                 . lines)
            . splitOn "\n\n"
            <$> getContents
  let est' :: [[Set.Set Pos]]
      est' = map fst
             $ takeWhile (not . null . fst)
             $ scanl stepFold ([Set.fromList (head probes)], (tail probes)) est'
      stepFold (_, remaining) sets =
        let results = scanl
              (\ (_, remaining) set -> matchesWith set remaining)
              ([], remaining)
              sets
        in (concatMap fst results, snd $ last results)
      est :: [Set.Set Pos]
      est = let est'' = concat est' in
              if length est'' < length probes
              then est''
                   -- error ("Couldn't resolve all probes," ++
                   --        " resolved: " ++ (show $ length est'') ++
                   --        " out of: " ++ (show $ length probes))
              else est''

      -- from p1 debug prints
      probePos = [(0,0,0),
               (38,1213,-102),
               (107,-1107,-103),
               (-39,1336,-1209),
               (-12,-1098,1067),
               (-59,-2331,-114),
               (-1221,1246,-1298),
               (-1199,-1205,1054),
               (-24,-2345,-1388),
               (1137,-2354,-185),
               (-1242,-2316,-20),
               (-2397,1350,-1273),
               (-1234,-19,-1347),
               (-1147,2470,-1367),
               (-1203,-2354,1028),
               (-1155,-2289,-1267),
               (-11,-2328,-2513),
               (-1212,-1125,-1217),
               (-1247,2471,-2553),
               (-1086,3712,-1326),
               (-2342,-2258,-1213),
               (-1087,-2382,-2510),
               (99,-1188,-2466),
               (-2422,-1229,-1332),
               (-1112,-1126,-2573),
               (-58,2432,-2497),
               (-2301,3723,-1377),
               (-2299,-3615,-1274),
               (-1137,-2335,-3610),
               (-2386,-1133,-2447),
               (-1077,-1232,-3760),
               (-1226,33,-3681)]
  -- print $ Set.size $ Set.unions est
  print $ maximum $ manhattan <$> probePos <*> probePos
  return ()
  where doMatch :: Set.Set Pos -> Set.Set Pos -> Maybe (Set.Set Pos)
        doMatch probeA probeB = fmap fst $ uncons $ do
          a <- Set.toList probeA
          b <- Set.toList probeB
          let mapped = Set.map ((.+a) . (.-b)) probeB
              hits = Set.intersection probeA mapped
          guard (Set.size hits >= 12)
          return $ trace (show (a.-b)) mapped

        tryMatch :: Set.Set Pos -> [Pos] -> Maybe (Set.Set Pos)
        tryMatch probeA rprobeB =
          foldl (<|>) Nothing
          $ map (doMatch probeA)
          $ map Set.fromList
          $ transpose
          $ map allOrientations rprobeB

        matchesWith :: Set.Set Pos -> [[Pos]] -> ([Set.Set Pos], [[Pos]])
        matchesWith probe restProbes =
          partitionEithers [maybe (Right rp) Left $ tryMatch probe rp | rp <- restProbes]
