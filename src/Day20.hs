import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Pos = (Int, Int)
type Algo = Array Int Bool
type Image = (Bool, Array Pos Bool)

addPos a b = zipPos (+) a b
neighbours a = map (addPos a)

main :: IO ()
main = do
  input <- getContents
  let [algorithm'', image'] = splitOn "\n\n" input
      algorithm' = map (=='#') $ concat $ lines algorithm''
      algorithm = listArray (0, pred $ length algorithm') algorithm'
      imageLines = transpose $ lines image'
      imageGrid = listArray ((1, 1), (length $ head imageLines, length imageLines))
        $ map (=='#') $ concat $ imageLines
      enhances = iterate (enhance algorithm) (False, imageGrid)
      scores = map (sum . fmap fromEnum . snd) enhances
  forM_ [2, 50] (print . (!!) scores)
  where enhance :: Algo -> Image -> Image
        enhance algo image@(neg, grid) =
          (bullshit && not neg,
           listArray newRange
           $ map ((algo!) . readBinBools . getAround image)
           $ range newRange)
          where bullshit = algo ! 0
                newRange = expand $ bounds grid

        expand :: (Pos, Pos) -> (Pos, Pos)
        expand ((xm, ym), (xM, yM)) = ((xm - 1, ym - 1), (xM + 1, yM + 1))

        neighbours :: Pos -> [Pos]
        neighbours pos = map (addPos pos) [(dx, dy) | dy <- [-1..1], dx <- [-1..1]]

        getAround :: Image -> Pos -> [Bool]
        getAround (dflt, image) pos =
          [if inRange (bounds image) p
           then image ! p
           else dflt
          | p <- neighbours pos]
