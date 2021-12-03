module Util where

import Data.List

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
splitOn s = splitS (isPrefixOf s)

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
