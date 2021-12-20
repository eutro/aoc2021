import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Pos = (Int, Int)
type Image = (Bool, Array Pos Bool)

addPos a b = zipPos (+) a b
neighbours a = map (addPos a)

main :: IO ()
main = do
  input <- getContents
  let [algorithm'', image'] = splitOn "\n\n" input
      algorithm' = map (=='#') $ concat $ lines algorithm''
      algorithm = listArray (0, pred $ length algorithm') algorithm'
      imageLines = lines image'
      imageGrid = listArray ((1, 1), (length $ head imageLines, length imageLines))
        $ map (=='#')
        $ concat
        $ transpose
        $ imageLines
        :: Array Pos Bool
      enhances = iterate (enhance algorithm) (False, imageGrid)
  print $ foldl (\a p -> a + fromEnum p) 0 $ snd $ enhances !! 2
  print $ foldl (\a p -> a + fromEnum p) 0 $ snd $ enhances !! 50
  return ()
  where enhance :: Array Int Bool -> Image -> Image
        enhance algo image@(neg, grid) =
          let bullshit = algo ! 0
              newRange = (mapP (addPos (-1,-1)) (addPos (1,1)) $ bounds grid)
          in (bullshit && not neg,
              listArray
              newRange
              [value
               | pos <- range newRange,
               let value =
                     (algo!)
                     $ readBinBools
                     $ getAround image pos])

        neighbours :: Pos -> [Pos]
        neighbours pos = map (addPos pos . swap) $ range ((-1,-1), (1,1))

        getAround :: Image -> Pos -> [Bool]
        getAround (dflt, image) pos =
          map
          (\p -> if inRange (bounds image) p
                 then image!p
                 else dflt)
          $ neighbours pos
