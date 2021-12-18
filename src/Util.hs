module Util
  (module Util,
   module Pairs)
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Debug.Trace
import Pairs

splitS :: (String -> Bool) -> String -> [String]
splitS pred s = case dropWhile pred (init $ tails s) of
  [] -> []
  s' -> (map head w) : s'''
    where
      (w, s'') = break pred s'
      s''' = case s'' of
               (s''':_) -> splitS pred s'''
               [] -> []

split :: (Char -> Bool) -> String -> [String]
split p = splitS (p . head)

splitOn :: String -> String -> [String]
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

toGrid :: Set.Set (Int, Int) -> String
toGrid points = unlines
                $ [[" #"!!(fromEnum $ (x, y) `Set.member` points)
                   | x <- [minx..maxx]] | y <- [miny..maxy]]
  where (minx, miny) = minimum points
        (maxx, maxy) = maximum points

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
