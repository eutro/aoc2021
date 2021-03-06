main :: IO ()
main = getContents
  >>= ($ (drop 3)) . ($ tail)
  . (($) >>= (.) . ($ (>>)) . (.) . flip (.))
  . (.) (putStrLn . show . length . filter id)
  . flip (>>= (flip $ zipWith (<)))
  . map (read :: String -> Int) . lines

main1 = do
  input <- getContents
  let lst = map read $ lines input :: [Int]
  let countInc f = putStrLn $ show $ length $ filter id $ zipWith (<) lst (f lst)
  countInc tail
  countInc (drop 3) -- two of the elements overlap in each window, only compare those that don't
