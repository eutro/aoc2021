import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as RP
import Bits

type Pos = (Int, Int)
data Dir = DownCC | RightCC deriving Eq
type Floor = Array Pos (Maybe Dir)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print
    $ succ
    $ length
    $ takeWhile (not . null)
    $ evalState (sequence $ repeat (stepFloor)) input
  return ()
  where parseInput :: String -> Floor
        parseInput str = listArray ((0, 0), (pred $ length $ head ls, pred $ length ls))
                         $ concat $ transpose grid :: Floor
          where ls = lines str
                grid = map (map readCc) ls
                readCc '>' = Just RightCC
                readCc 'v' = Just DownCC
                readCc _ = Nothing

        printFloor :: Floor -> String
        printFloor floor = unlines [[tileToChar (floor ! (x, y)) | x <- [xm..xM]] | y <- [ym..yM]]
          where ((xm, ym), (xM, yM)) = bounds floor
                tileToChar Nothing = '.'
                tileToChar (Just RightCC) = '>'
                tileToChar (Just DownCC) = 'v'

        traceFloor :: State Floor ()
        traceFloor = do
          floor <- get
          trace (printFloor floor) $ return ()

        posAdd :: Floor -> Pos -> Pos -> Pos
        posAdd floor (x, y) (dx, dy) = ret
          where ret = ((x + dx) `mod` (succ w), (y + dy) `mod` (succ h))
                (_, (w, h)) = bounds floor

        stepDir :: Floor -> Dir -> Pos -> [(Pos, Maybe Dir)]
        stepDir floor dir delta = do
          (pos, v) <- assocs floor
          guard (v == Just dir)
          let np = posAdd floor pos delta
          guard (isNothing $ floor ! np)
          [(pos, Nothing), (np, v)]

        stepFloor :: State Floor [(Pos, Maybe Dir)]
        stepFloor = do
          floor <- get
          let stepRs = stepDir floor RightCC (1, 0)
              floor' = floor // stepRs
              stepDs = stepDir floor' DownCC (0, 1)
          put (floor' // stepDs)
          return (stepRs ++ stepDs)
