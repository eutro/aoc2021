import Text.ParserCombinators.ReadP
import Data.Char
import Data.List
import Debug.Trace
import qualified Data.Map as Map

type Psp = Map.Map String String

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

parseAllPsps :: String -> [Psp]
parseAllPsps s = do
  rawpsp <- splitOn "\n\n" s
  return $ Map.fromList $ do
    rawfield <- split isSpace rawpsp
    return (take 3 rawfield, drop 4 rawfield)

valid1 :: Psp -> Bool
valid1 psp = all (`Map.member` psp) ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] -- cid

main :: IO ()
main = do
  input <- getContents
  let psps = parseAllPsps input
  putStrLn $ show $ length $ filter valid1 psps
