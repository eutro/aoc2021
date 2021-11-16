import Text.ParserCombinators.ReadP
import Data.Char

parseL :: String -> (Int, Int, Char, String)
parseL = fst . head . readP_to_S
         (do x <- munch isDigit
             char '-'
             y <- munch isDigit
             char ' '
             c <- get
             string ": "
             s <- look
             return (read x, read y, c, s))

verify1 :: (Int, Int, Char, String) -> Bool
verify1 (x, y, c, s) =
  let count = length (filter ((==) c) s) in
    x <= count && count <= y

verify2 :: (Int, Int, Char, String) -> Bool
verify2 (x, y, c, s) =
  length (filter ((==) c) [s!!(x-1), s!!(y-1)]) == 1

main :: IO ()
main = do input <- getContents
          let records = map parseL (lines input)
              v1 = length (filter verify1 records)
              v2 = length (filter verify2 records) in
            do putStrLn (show v1)
               putStrLn (show v2)
