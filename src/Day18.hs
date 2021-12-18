import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
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

data SnF = Pair (SnF, SnF) | Single Int

instance Show SnF where
  show (Pair (l, r)) = "["++(show l)++","++(show r)++"]"
  show (Single i) = show i

instance Read SnF where
  readsPrec _ = RP.readP_to_S $ fix $ \ readSnF ->
    RP.between (RP.char '[') (RP.char ']')
    (curry Pair <$> (readSnF <* (RP.char ',')) <*> readSnF)
    RP.<++ fmap Single (RP.readS_to_P reads :: RP.ReadP Int)

updateSnF :: (SnF -> SnF) -> [PairSide] -> SnF -> SnF
updateSnF f [] x = f x
updateSnF f (s:path) (Pair p) = Pair $ updateP (updateSnF f path) s p

getSnF :: [PairSide] -> SnF -> SnF
getSnF [] x = x
getSnF (s:path) (Pair p) = getSnF path $ getP s p

findLeftMost (Single _) = []
findLeftMost (Pair (l, _)) = PairLeft : findLeftMost l

findRightMost (Single _) = []
findRightMost (Pair (_, r)) = PairRight : findRightMost r

isSingle (Single _) = True
isSingle _ = False

getSingle (Single x) = x

mapSingle f (Single x) = Single (f x)

findExplode path left right (Single _) = Nothing
findExplode path left right (Pair (l, r)) =
  if length path >= 4 && isSingle l && isSingle r
  then Just (reverse <$> left, reverse path, reverse <$> right)
  else let lf = findExplode (PairLeft:path) left (Just $ findLeftMost r ++ PairRight:path) l
           rf = findExplode (PairRight:path) (Just $ findRightMost l ++ PairLeft:path) right r
       in if isJust lf then lf else rf

findSplit path (Single x) = if x >= 10 then Just $ reverse path else Nothing
findSplit path (Pair (l, r)) =
  let lf = findSplit (PairLeft:path) l
      rf = findSplit (PairRight:path) r
  in if isJust lf then lf else rf

normalise :: SnF -> SnF
normalise snf =
  case findExplode [] Nothing Nothing snf of
    Just (left, path, right) -> normalise $
      let Pair (Single lv, Single rv) = getSnF path snf
      in fromMaybe id (updateSnF (mapSingle (lv+)) <$> left)
         $ fromMaybe id (updateSnF (mapSingle (rv+)) <$> right)
         $ updateSnF (const $ Single 0) path snf
    Nothing -> case findSplit [] snf of
      Just path -> normalise $ updateSnF
                   (\ (Single n) -> Pair (Single $ n `div` 2, Single $ (n `div` 2) + (n `mod` 2)))
                   path snf
      Nothing -> snf

addSnF :: SnF -> SnF -> SnF
addSnF = curry $ normalise . Pair

mag :: SnF -> Integer
mag (Pair (l, r)) = 3*(mag l) + 2*(mag r)
mag (Single i) = toInteger i

main :: IO ()
main = do
  input <- getContents
  let snfs = map read $ lines input
  print $ mag $ foldl1 addSnF snfs
  print $ maximum $ map mag $ addSnF <$> snfs <*> snfs
  return ()
