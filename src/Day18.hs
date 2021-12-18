import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
import Control.Applicative
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

normalise :: SnF -> SnF
normalise snf =
  fromMaybe snf
  $ appExplode <$> findExplode [] Nothing Nothing snf
  <|> appSplit <$> findSplit [] snf
  where appExplode (left, path, right) = normalise $
          let Pair (Single lv, Single rv) = getSnF path snf
          in fromMaybe id (updateSnF (mapSingle (lv+)) <$> left)
             $ fromMaybe id (updateSnF (mapSingle (rv+)) <$> right)
             $ updateSnF (const $ Single 0) path snf

        mapSingle f (Single x) = Single (f x)

        appSplit path = normalise $ updateSnF createSplit path snf

        createSplit (Single n) =
          (curry Pair `on` Single)
          (n `div` 2)
          ((n `div` 2) + (n `mod` 2))

        isSingle (Single _) = True
        isSingle _ = False

        findLeftMost (Single _) = []
        findLeftMost (Pair (l, _)) = PairLeft : findLeftMost l

        findRightMost (Single _) = []
        findRightMost (Pair (_, r)) = PairRight : findRightMost r

        findExplode path left right (Single _) = Nothing
        findExplode path left right (Pair (l, r))
          | length path >= 4 && isSingle l && isSingle r =
            Just (reverse <$> left, reverse path, reverse <$> right)
          | otherwise =
            findExplode (PairLeft:path) left (Just $ findLeftMost r ++ PairRight:path) l
            <|> findExplode (PairRight:path) (Just $ findRightMost l ++ PairLeft:path) right r

        findSplit path (Single x)
          | x >= 10 = Just $ reverse path
          | otherwise = Nothing
        findSplit path (Pair (l, r)) =
          findSplit (PairLeft:path) l
          <|> findSplit (PairRight:path) r

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
