import qualified Text.ParserCombinators.ReadP as RP
import Bits

data Tree a = Pair (Tree a, Tree a) | Single a
type SnF = Tree Int

instance Show a => Show (Tree a) where
  show (Pair (l, r)) = "["++(show l)++","++(show r)++"]"
  show (Single i) = show i

instance Read a => Read (Tree a) where
  readsPrec _ = RP.readP_to_S readTree
    where readTree = readPair RP.<++ readSingle
          readPair = do
            RP.char '['
            lhs <- readTree
            RP.char ','
            rhs <- readTree
            RP.char ']'
            return $ Pair (lhs, rhs)
          readSingle = Single <$> (RP.readS_to_P reads)

instance Functor Tree where
  fmap f (Single x) = Single (f x)
  fmap f (Pair (l, r)) = Pair (fmap f l, fmap f r)

updateSnF :: (SnF -> SnF) -> [PairSide] -> SnF -> SnF
updateSnF f [] x = f x
updateSnF f (s:path) (Pair p) = Pair $ updateP (updateSnF f path) s p

normalise :: SnF -> SnF
normalise snf = explode <|> split & fromMaybe snf
  where explode :: Maybe SnF
        explode = findExplode [] Nothing Nothing snf

        findExplode :: [PairSide]
                    -> Maybe [PairSide]
                    -> Maybe [PairSide]
                    -> SnF
                    -> Maybe SnF
        findExplode path left right (Single _) = Nothing
        findExplode path left right (Pair (Single lv, Single rv)) = do
          guard (length path >= 4)
          Just $ normalise
            $ fromMaybe id (updateSnF (fmap (lv+)) <$> reverse <$> left)
            $ updateSnF (const $ Single 0) (reverse path)
            $ fromMaybe id (updateSnF (fmap (rv+)) <$> reverse <$> right)
            snf
        findExplode path left right (Pair (l, r)) =
          findExplode (PairLeft:path) left (Just $ findXMost PairLeft r ++ PairRight:path) l
          <|> findExplode (PairRight:path) (Just $ findXMost PairRight l ++ PairLeft:path) right r

        findXMost :: PairSide -> SnF -> [PairSide]
        findXMost side (Single _) = []
        findXMost side (Pair pair) = side : findXMost side (getP side pair)

        split :: Maybe SnF
        split = findSplit [] snf

        findSplit :: [PairSide] -> SnF -> Maybe SnF
        findSplit path (Single n) = do
          guard (n >= 10)
          let half = n `div` 2
          Just $ normalise
            $ updateSnF
            (const (on (curry Pair) Single half (n - half)))
            (reverse path)
            snf
        findSplit path (Pair (l, r)) =
          findSplit (PairLeft:path) l
          <|> findSplit (PairRight:path) r

addSnF :: SnF -> SnF -> SnF
addSnF = curry $ normalise . Pair

mag :: SnF -> Int
mag (Pair (l, r)) = 3*(mag l) + 2*(mag r)
mag (Single i) = i

main :: IO ()
main = do
  input <- getContents
  let snfs = map read $ lines input
  print $ mag $ foldl1 addSnF snfs
    -- note: this adds elements to themselves, it's by luck that such a pair is not the maximum
  print $ maximum $ map mag $ addSnF <$> snfs <*> snfs
  return ()
