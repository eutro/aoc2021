import qualified Data.Set as Set
import Bits

type SState = ((Int, Int), (Int, Int)) -- pos, vel

main :: IO ()
main = do
  input <- getLine
  let [[minx, maxx], [miny, maxy]] =
        map (map read . splitOn ".." . drop 2)
        $ splitOn ", "
        $ drop (length "target area: ") input :: [[Int]]
      area = ((minx, miny), (maxx, maxy))
      dycap = pred (-miny)
  print $ triangle dycap
  print $ Set.size $ Set.fromList $ concatMap (totalY area) $ range (miny, dycap)
  return ()
  where tryXY :: SState -> (Int, Int) -> Bool
        tryXY area@((minx, miny), (maxx, maxy)) vel = loop ((0, 0), vel)
          where loop s@(pos@(x, y), (dx, _))
                  | inRange area pos = True
                  | y < miny || x > maxx = False
                  | dx == 0 && x < minx = False
                  | otherwise = loop (step s)

        step :: SState -> SState
        step ((x, y), (dx, dy)) =
          ((x + dx, y + dy), (dx - sign dx, dy - 1))

        totalY :: ((Int, Int), (Int, Int)) -> Int -> [(Int,Int)]
        totalY area@((xm, ym), (xM, yM)) j = is
          where a = (2*(fromIntegral j) + 1)
                f yv = (a + (sqrt $ (a^2) - 8 * yv)) / 2
                nm = ceiling $ f $ fromIntegral yM
                nM = floor $ f $ fromIntegral ym
                is = map ((,) j) $ concatMap (possibleXs area) [nm..nM]

        possibleXs :: ((Int, Int), (Int, Int)) -> Int -> [Int]
        possibleXs area@((xm, ym), (xM, yM)) n = slowIs ++ fastIs
          where a = (fromIntegral n - 1) / 2
                im = max n $ ceiling $ fromIntegral xm / fromIntegral n + a
                iM = floor $ fromIntegral xM / fromIntegral n + a
                f xv = (-1/2) + (sqrt $ 2*xv + 1/4)
                sim = ceiling $ f $ fromIntegral xm
                siM = min n $ floor $ f $ fromIntegral xM
                fastIs = [im..iM]
                slowIs = [sim..siM]
