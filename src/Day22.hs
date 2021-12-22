import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as RP
import Bits

type Pos3 = (Int, Int, Int)
type Range3 = (Pos3, Pos3)

data Insn = Insn
  { isOn :: Bool
  , insnRange :: Range3 }
  deriving (Eq, Ord, Show)

instance Read Insn where
  readsPrec _ str = [(Insn (onOrOff == "on") ((xm, ym, zm), (xM, yM, zM)), "")]
    where [onOrOff, rawRanges] = splitOn " " str
          [[xm, xM],
           [ym, yM],
           [zm, zM]] =
            map (map read . splitOn ".." . drop 2) $ splitOn "," rawRanges

main :: IO ()
main = do
   insns <- map read . lines <$> getContents :: IO [Insn]
   let smallInsns = takeWhile (inRange smallRangeCube . fst . insnRange) insns
       smallFilled = foldl
                     fillSmall
                     (listArray
                      smallRangeCube
                      [False | _ <- range smallRangeCube])
                     smallInsns
   print $ sum $ fromEnum <$> smallFilled
   print $ sumFill $ foldl fillLarge Map.empty insns
   return ()
   where smallRange = (-50, 50)
         smallRangeCube = (mapBoth (join (join (,,))) smallRange)

         fillSmall :: FoldFn (Array Pos3 Bool) Insn
         fillSmall grid insn = grid // [(pos, isOn insn) | pos <- range $ insnRange insn]

         intersection :: Range3 -> Range3 -> Maybe Range3
         intersection
           ((axm, aym, azm), (axM, ayM, azM))
           ((bxm, bym, bzm), (bxM, byM, bzM)) = do
           [(xm, xM), (ym, yM), (zm, zM)] <-
             mapM intersectAxis [((axm, axM), (bxm, bxM)),
                                 ((aym, ayM), (bym, byM)),
                                 ((azm, azM), (bzm, bzM))]
           return ((xm, ym, zm), (xM, yM, zM))
           where intersectAxis :: ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
                 intersectAxis ((am, aM), (bm, bM)) =
                   guard (nm <= nM) >> return (nm, nM)
                   where nm = max am bm
                         nM = min aM bM

         rangeSize :: Range3 -> Integer
         rangeSize ((xm, ym, zm), (xM, yM, zM)) = toInteger $ w*h*l
           where w = xM - xm + 1
                 h = yM - ym + 1
                 l = zM - zm + 1

         sumFill :: Map.Map Range3 Int -> Integer
         sumFill m = sum $ uncurry (*) <$> mapP rangeSize toInteger <$> Map.toList m

         fillLarge :: FoldFn (Map.Map Range3 Int) Insn
         fillLarge ranges insn = prependRange
           $ foldl (flip $ uncurry $ Map.insertWith (+)) ranges $ do
           (r, s) <- Map.toList ranges
           r' <- maybeToList $ intersection (insnRange insn) r
           return (r', -s)
           where prependRange
                   | isOn insn = Map.insertWith (+) (insnRange insn) 1
                   | otherwise = id
