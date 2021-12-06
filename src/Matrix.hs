module Matrix where

import Data.List

dot :: Num a => [a] -> [a] -> a
a `dot` b = sum $ zipWith (*) a b

mxv :: Num a => [[a]] -> [a] -> [a]
m `mxv` v = map (dot v) m

mxm :: Num a => [[a]] -> [[a]] -> [[a]]
a `mxm` b = map (mxv $ transpose b) a

msquare :: Num a => [[a]] -> [[a]]
msquare a = a `mxm` a

mpow :: Num a => Integer -> [[a]] -> [[a]]
mpow 1 m = m
mpow n m
  | even n = msquare $ mpow (n `div` 2) m
  | otherwise = mxm m (mpow (n - 1) m)
