-- 2015 d2

import Util

import Data.List

main :: IO ()
main = do
  cts <- getContents
  let anss = do
        line <- lines cts
        let [l, w, h] = sort $ map read $ splitOn "x" line :: [Int]
        let [a, b, c] = [l*w, w*h, h*l]
        return (2*(a+b+c) + a,
                2*(l+w) + l*w*h)
  putStrLn $ show $ sum $ map (\(x,_)->x) anss
  putStrLn $ show $ sum $ map (\(_,x)->x) anss
