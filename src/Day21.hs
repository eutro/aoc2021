import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

data Player = Player {remaining::Int,pos::Int} deriving (Eq, Ord, Ix, Show)

setRem :: Int -> Player -> Player
setRem x (Player _ p) = Player x p

stepPlayer :: Int -> Player -> Player
stepPlayer n (Player r p) = Player (r - np) np
  where np = mod1 (p + n) 10

mod1 :: Int -> Int -> Int
mod1 a b = succ $ (`mod` b) $ pred a

main :: IO ()
main = do
  players <- parseInput <$> getContents
  print $ maximum $ pToList $ countWins $ listToP $ map (setRem 21) players
  return ()
  where parseInput :: String -> [Player]
        parseInput input = do
          line <- lines input
          let pos = read $ drop (length "Player 1 starting position: ") line
          return $ Player 0 pos

        countWins :: (Player, Player) -> (Integer, Integer)
        countWins (p1, p2) = (winsFor (p1, p2, True), winsFor (p1, p2, False))

        winsFor :: (Player, Player, Bool) -> Integer
        winsFor idx = (!idx) $ listArray bounds $ map compute $ range bounds
          where compute (p1, p2, isP1) = sum $ do
                  (roll, freq) <- tripleRolls
                  let p1n = stepPlayer roll p1
                  return $ if remaining p1n <= 0
                    then [0, freq]!!(fromEnum isP1)
                    else freq * winsFor (p2, p1n, not isP1)

                bounds = ((minPlayer, minPlayer, False), (maxPlayer, maxPlayer, True))
                minPlayer = Player 1 1
                maxPlayer = Player 21 10

        tripleRolls :: [(Int, Integer)]
        tripleRolls = Map.toList $ frequencies $ add3 <$> die <*> die <*> die
          where die = [1,2,3]
                add3 x y z = x + y + z
