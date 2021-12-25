import qualified Data.Set as Set
import Bits

main :: IO ()
main = do
  input <- getLine
  let [[minx, maxx], [miny, maxy]] =
        map (map read . splitOn ".." . drop 2)
        $ splitOn ", "
        $ drop (length "target area: ") input :: [[Int]]
  -- assuming (miny, maxy < 0),
  -- the highest start y vel is the one that just about clips the bottom;
  -- with a positive starting y velocity j,
  -- when the probe reaches y=0 again, its velocity will be (-j - 1),
  -- which means it will hit (-j - 1) on the next step;
  -- we want to clip m_y with this, so solve:
  --    -j - 1 = m_y
  -- => -j = m_y + 1
  -- => j = -m_y - 1
  -- the max height reached by the probe is simply the sum from j to 0
  -- i.e. the jth triangle number == n(n+1)/2
  print $ triangle $ pred (-miny)
  print $ totalInArea ((minx, miny), (maxx, maxy))
  where totalInArea :: ((Int, Int), (Int, Int)) -> Int
        totalInArea area@((xm, ym), (xM, yM)) =
          -- the range of start js is [ym, -ym),
          -- the latter as discussed before,
          -- the former because anything lower will simply
          -- fall below the target area on step 1;
          -- so for each j in this range, compute how many
          -- starting x velocities will have the probe be in the area
          sum $ map totalY $ range (ym, pred (-ym))
          where totalY :: Int -> Int
                totalY j
                  -- for each starting velocity j,
                  -- the position y at any time n
                  -- is just a quadratic in n:
                  --   \sum_{r=0}^{n-1}(j - r)
                  -- = jn - \sum_{r=0}^{n-1}{r}
                  -- = jn - n(n-1)/2
                  -- = jn - (n^2 - n)/2
                  -- = (2nj - n^2 - n)/2
                  -- = (-n^2 + (2j + 1)n)/2
                  -- = (-1/2)n^2 + ((2j + 1)/2)n
                  -- then we can solve for when it falls in the range,
                  -- which is computed by nBound below;
                  -- finally, knowing for which ns the probe lands in the target area,
                  -- we just have to find the range of xs for which it does so
                  | nm <= nM = maybe 0 rangeSpan
                               $ foldl foldMinMax Nothing
                               $ mapMaybe possibleXs [nm, nM]
                  | otherwise = 0
                  where a = (2*(fromIntegral j) + 1)
                        nBound yv = (a + (sqrt $ (a^2) - 8 * (fromIntegral yv))) / 2
                        nm = ceiling $ nBound yM
                        nM = floor $ nBound ym

                rangeSpan :: (Int, Int) -> Int
                rangeSpan (nm, nM) = succ $ nM - nm
                foldMinMax :: FoldFn (Maybe (Int, Int)) (Int, Int)
                foldMinMax Nothing minMax = Just minMax
                foldMinMax (Just (om, oM)) (nm, nM) = Just (min om nm, max oM nM)

                possibleXs = computeX `memoisedOver` (1, 2*(-ym))
                  -- there's two cases here:

                  -- if the starting x velocity i is less than or equal to n;
                  -- the probe will no longer be moving right, because of drag;
                  -- there's a fixed range of is which land in the area this way;
                  -- this is also a quadratic, computed once by slowIBound below;

                  -- if i is greater than n, it has not stopped moving yet;
                  -- the x position is a quadratic in n (the same as the one for y),
                  -- but n is fixed, so we only have to solve the linear equation for i,
                  -- which is computed by fastIBound below
                  where computeX n = guard (im <= iM) >> Just (im, iM)
                          where a = (fromIntegral n - 1) / 2
                                fastIBound xv = a + fromIntegral xv / fromIntegral n
                                fim = ceiling $ fastIBound xm
                                fiM = floor $ fastIBound xM
                                im = if n >= sim then sim else max n fim
                                iM = if n <= fiM then fiM else min n siM

                        slowIBound xv = (-1/2) + (sqrt $ 2*(fromIntegral xv) + 1/4)
                        sim = ceiling $ slowIBound xm
                        siM = floor $ slowIBound xM
