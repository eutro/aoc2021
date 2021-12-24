{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Util
  (module Util,
   module Pairs)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Arr
import Data.Ix
import Data.List
import Data.Function
import Debug.Trace
import Pairs

splitS :: ([a] -> Bool) -> [a] -> [[a]]
splitS pred s = case dropWhile pred (init $ tails s) of
  [] -> []
  s' -> (map head w) : s'''
    where
      (w, s'') = break pred s'
      s''' = case s'' of
               (s''':_) -> splitS pred s'''
               [] -> []

split :: (a -> Bool) -> [a] -> [[a]]
split p = splitS (p . head)

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn ss s = let (h : tail) = splitS (isPrefixOf ss) s
               in h : (map (drop (length ss - 1)) tail)

readBase :: Int -> [Int] -> Int
readBase base digits = foldl ((+) . (* base)) 0 digits

readBinDigits :: [Int] -> Int
readBinDigits = readBase 2

readBinBools :: [Bool] -> Int
readBinBools = readBinDigits . map fromEnum

readBinChars :: [Char] -> Int
readBinChars =  readBinBools . map (== '1')

if' :: Bool -> a -> a -> a
if' True ifTrue _ = ifTrue
if' False _ ifFalse = ifFalse

binarySearch :: Integral a => (a -> Ordering) -> a -> a -> a
binarySearch f mn mx =
  if mn >= mx
  then mn
  else let mid = (mn + mx) `div` 2 in
         case f mid of
           LT -> binarySearch f mn (mid - 1)
           EQ -> mid
           GT -> binarySearch f (mid + 1) mx

dbg :: Show a => a -> a
dbg = show >>= trace

disjoin :: [a -> Bool] -> a -> Bool
disjoin preds x = any ($ x) preds

conjoin :: [a -> Bool] -> a -> Bool
conjoin preds x = all ($ x) preds

frequencies :: (Ord a, Num n) => [a] -> (Map.Map a n)
frequencies ls = Map.fromListWith (+) $ map (\ x -> (x, 1)) ls

class UtilGrid a where
  gridBounds :: a -> ((Int, Int), (Int, Int))
  gridHasPos :: a -> (Int, Int) -> Bool
  gridGetOrDflt :: a -> Bool -> (Int, Int) -> Bool
  gridGetOrDflt grid dflt pos
    | inRange (gridBounds grid) pos = gridHasPos grid pos
    | otherwise = dflt

type SparseGrid = Set.Set (Int, Int)
type DenseGrid = Arr.Array (Int, Int) Bool

instance UtilGrid SparseGrid where
  gridBounds grid = ((minx, miny), (maxx, maxy))
    where (minx, _) = minimumBy (compare `on` fst) grid
          (maxx, _) = maximumBy (compare `on` fst) grid
          (_, miny) = minimumBy (compare `on` snd) grid
          (_, maxy) = maximumBy (compare `on` snd) grid
  gridHasPos grid pos = pos `Set.member` grid

instance UtilGrid DenseGrid where
  gridBounds = Arr.bounds
  gridHasPos = (Arr.!)

toGrid :: UtilGrid a => a -> String
toGrid grid = unlines
              $ [[".#"!!(fromEnum $ grid `gridHasPos` (x, y))
                 | x <- [minx..maxx]] | y <- [miny..maxy]]
  where ((minx, miny), (maxx, maxy)) = gridBounds grid

minMax :: (Foldable t, Ord a) => t a -> (a, a)
minMax l = (minimum l, maximum l)

iterateM :: Monad m => (a -> m a) -> a -> m a
iterateM f = g where g x = f x >>= g

sign :: Int -> Int
sign 0 = 0
sign x
  | x < 0 = -1
  | x > 0 = 1

triangle :: Integral a => a -> a
triangle n = div (n * (n + 1)) 2

median :: Ord a => [a] -> a
median l = sort l !! div (length l) 2

type FoldFn a b = a -> b -> a

mod1 :: Int -> Int -> Int
mod1 a b = succ $ (`mod` b) $ pred a
