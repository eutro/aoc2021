import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array.ST
import Data.Array.MArray
import Control.Monad.ST
import Bits

type Pos = (Int, Int)
type Algo = Array Int Bool
type Image = Array Pos Bool

main :: IO ()
main = do
  (algo, image) <- parseInput <$> getContents
  forM_ [2, 50] (print . (enhanceSeq algo image !!))
  where parseInput :: String -> (Algo, Image)
        parseInput input =
          (listArray (0, pred $ length algoList) algoList,
           listArray imageBounds $ map (=='#') $ concat $ imageLines)
          where [algoRaw, imageRaw] = splitOn "\n\n" input
                algoList = map (=='#') $ concat $ lines algoRaw
                imageLines = transpose $ lines imageRaw
                imageBounds = ((1, 1), (length $ head imageLines, length imageLines))

        enhanceSeq :: Algo -> Image -> [Int]
        enhanceSeq algo startImage =
          evalState doEnhances startImage
          where doEnhances :: State Image [Int]
                doEnhances = traverse enhanceOnce [0..]
                enhanceOnce :: Int -> State Image Int
                enhanceOnce index = do
                  arr <- get
                  let dflt = algo ! 0 && index `mod` 2 == 1
                      inBounds = bounds arr
                      outBounds = expand inBounds
                      getPos pos =
                        if inRange inBounds pos
                        then arr ! pos
                        else dflt
                  put $ listArray outBounds $ do
                    pos <- (range outBounds)
                    let surrounding = map getPos (neighbours pos)
                    return $ (algo!) $ readBinBools surrounding
                  return $
                    if dflt
                    then undefined
                    else sum $ fmap fromEnum arr

        expand :: (Pos, Pos) -> (Pos, Pos)
        expand ((xm, ym), (xM, yM)) = ((xm - 1, ym - 1), (xM + 1, yM + 1))

        neighbours :: Pos -> [Pos]
        neighbours pos = map (zipPos (+) pos) [(dx, dy) | dy <- [-1..1], dx <- [-1..1]]
