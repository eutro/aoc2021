module Util where

import Data.List
import Debug.Trace

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
