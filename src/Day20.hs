{-# LANGUAGE BinaryLiterals #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

type Pos = (Int, Int)
type Algo = Array Int Bool
type BigAlgo = Array Int [Bool]
type Image = Array Pos Bool

main :: IO ()
main = do
  (algo, image) <- parseInput <$> getContents
  forM_ [2, 50] $ print . imageScore . (enhanceSeq algo image !!)
  where parseInput :: String -> (Algo, Image)
        parseInput input = (algo, image)
          where algo = listArray (0, pred $ length algoList) algoList
                algoList = map (=='#') $ concat $ lines algoRaw
                image = listArray imageBounds $ map (=='#') $ concat $ imageLines
                imageBounds = ((1, 1), (length $ head imageLines, length imageLines))
                imageLines = transpose $ lines imageRaw
                [algoRaw, imageRaw] = splitOn "\n\n" input

        enlargeAlgo :: Algo -> BigAlgo
        enlargeAlgo algo = listArray domain $ map compute $ range domain
          where domain = (0, pred $ 2^16)
                compress x =
                  (0b0111 .&. x) .|.
                  ((0b1110000 .&. x) `shiftR` 1) .|.
                  ((0b11100000000 .&. x) `shiftR` 2)
                compute short = (algo!) . compress <$> (shiftR <$> [short] <*> [5,4,1,0])

        imageScore :: Image -> Int
        imageScore image = fromEnum <$> image & sum

        enhanceSeq :: Algo -> Image -> [Image]
        enhanceSeq algo startImage =
          evalState doEnhances normImage
          where normImage
                  | even w && even h = startImage
                  | otherwise = listArray newRange
                                $ map (gridGetOrDflt startImage False)
                                $ range newRange
                  where ((xm, ym), (xM, yM)) = bounds startImage
                        w = xM - xm + 1
                        h = yM - ym + 1
                        nxM = if even w then xM else succ xM
                        nyM = if even h then yM else succ yM
                        newRange = ((xm, ym), (nxM, nyM))
                bigAlgo = enlargeAlgo algo
                doEnhances :: State Image [Image]
                doEnhances = traverse enhanceOnce [0..]
                enhanceOnce :: Int -> State Image Image
                enhanceOnce index = do
                  arr <- get
                  let dflt = algo ! 0 && index `mod` 2 == 1
                      inBounds = bounds arr
                      outBounds@((xm, ym), (xM, yM)) = expand inBounds
                      getPos pos =
                        if inRange inBounds pos
                        then arr ! pos
                        else dflt
                  put $ array outBounds $ do
                    pos@(x, y) <- (,) <$> [xm,xm+2..xM] <*> [ym,ym+2..yM]
                    let surrounding = map getPos $ neighbourhood (x, y)
                    zip [(x,y),(x+1,y),(x,y+1),(x+1,y+1)]
                      $ bigAlgo ! readBinBools surrounding
                  return arr

        expand :: (Pos, Pos) -> (Pos, Pos)
        expand ((xm, ym), (xM, yM)) = ((xm - 1, ym - 1), (xM + 1, yM + 1))

        neighbourhood :: Pos -> [Pos]
        neighbourhood pos = map (zipPos (+) pos) [(dx, dy) | dy <- [-1..2], dx <- [-1..2]]
