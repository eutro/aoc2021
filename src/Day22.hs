import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as RP
import Bits
import Triple

type AxisRange = (Int, Int)
type Range f = f AxisRange
type Range3 = Range Triple

class Traversable r => RangeOps r where
  rangeFromList :: [a] -> r a
  firstAxis :: r a -> a
  rangeZip :: r a -> r a -> r (a, a)

instance RangeOps [] where
  rangeFromList = id
  firstAxis = head
  rangeZip = zip

-- triple provides a significant performance boost over list,
-- while still using generic ops
instance RangeOps Triple where
  rangeFromList [a, b, c] = Triple a b c
  firstAxis (Triple a _ _) = a
  rangeZip (Triple a1 b1 c1) (Triple a2 b2 c2) = Triple (a1, a2) (b1, b2) (c1, c2)

data Insn = Insn { isOn :: Bool , insnRange :: Range3 } deriving (Eq, Ord, Show)

instance Read Insn where
  readsPrec _ str = [(Insn (onOrOff == "on") insnRanges, "")]
    where [onOrOff, rawRanges] = splitOn " " str
          insnRanges = rangeFromList
            $ map (listToP . map read . splitOn ".." . drop 2)
            $ splitOn "," rawRanges

main :: IO ()
main = do
   insns <- map read . lines <$> getContents :: IO [Insn]
   let (smallInsns, largeInsns)
         = span (inRange (-50, 50) . fst . firstAxis . insnRange) insns
   mapM_ (print . sumFill)
     $ tail $ scanl (foldl fillLarge) Map.empty [smallInsns, largeInsns]
   where intersection :: Range3 -> Range3 -> Maybe Range3
         intersection a b = mapM intersectAxis $ rangeZip a b
           where intersectAxis :: ((Int, Int), (Int, Int)) -> Maybe (Int, Int)
                 intersectAxis ((am, aM), (bm, bM)) =
                   guard (nm <= nM) >> return (nm, nM)
                   where nm = max am bm
                         nM = min aM bM

         rangeSize :: Range3 -> Integer
         rangeSize range = product $ toInteger . succ . uncurry subtract <$> range

         sumFill :: Map.Map Range3 Int -> Integer
         sumFill ranges = sum $ uncurry (*) <$> mapP rangeSize toInteger <$> Map.toList ranges

         fillLarge :: FoldFn (Map.Map Range3 Int) Insn
         fillLarge ranges (Insn isOn insnRange) = prependRange
           $ foldl (flip $ uncurry $ Map.insertWith (+)) ranges $ do
           (r, s) <- Map.toList ranges
           r' <- maybeToList $ intersection insnRange r
           return (r', -s)
           where prependRange
                   | isOn = Map.insertWith (+) insnRange 1
                   | otherwise = id
