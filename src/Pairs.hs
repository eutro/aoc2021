module Pairs where

data PairSide = PairLeft | PairRight deriving Show

zipPos :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipPos f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

otherSide :: PairSide -> PairSide
otherSide PairLeft = PairRight
otherSide PairRight = PairLeft

axisPSide :: String -> PairSide
axisPSide "x" = PairLeft
axisPSide "y" = PairRight

getP :: PairSide -> (a, a) -> a
getP PairLeft (l, _) = l
getP PairRight (_, r) = r

updateP :: (a -> a) -> PairSide -> (a, a) -> (a, a)
updateP f PairLeft (l, r) = (f l, r)
updateP f PairRight (l, r) = (l, f r)

mapP :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapP f g (x, y) = (f x, g y)

mapLeft :: (a -> b) -> (a, c) -> (b, c)
mapLeft f (x, y) = (f x, y)

mapRight :: (b -> c) -> (a, b) -> (a, c)
mapRight f (x, y) = (x, f y)

listToP :: [a] -> (a, a)
listToP [x, y] = (x, y)

pToList :: (a, a) -> [a]
pToList (x, y) = [x, y]
