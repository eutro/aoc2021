import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

data Player = Player {remaining::Int,pos::Int} deriving (Eq, Ord, Ix, Show)

main :: IO ()
main = do
  players <- parsePlayers <$> getContents
  print $ uncurry (*) $ mapRight (1000-) $ deterministicDie $ players 1000
  print $ uncurry max $ quantumDie $ players 21
  return ()
  where parsePlayers :: String -> Int -> (Player, Player)
        parsePlayers input = makePlayers
          where makePlayers scoreToWin = listToP $ Player <$> [scoreToWin] <*> starts
                starts = read . drop (length "Player N starting position: ")
                         <$> take 2 (lines input)

        deterministicDie :: (Player, Player) -> (Int, Int)
        deterministicDie start = (totalRolls, loserRem)
          where totalRolls = 3 * length unwonStates
                loserRem = remaining $ snd (last unwonStates)

                unwonStates = takeWhile unwon stateSeq
                  where unwon (_, p2) = remaining p2 > 0

                stateSeq = scanl stepState start tripleRolls
                  where stepState :: FoldFn (Player, Player) Int
                        stepState (p1, p2) roll = (p2, stepPlayer roll p1)

                tripleRolls = map (sum . take 3) $ iterate (drop 3) singleRolls
                  where singleRolls = cycle [1..100]

        quantumDie :: (Player, Player) -> (Integer, Integer)
        quantumDie = compute `memoisedOver` playerDomain
          where compute (p1, p2) = sumPos $ map computeRoll tripleRolls
                  where computeRoll (roll, freq)
                          | remaining p1' <= 0 = (freq, 0)
                          | otherwise = mapBoth (freq*) $ swap $ quantumDie (p2, p1')
                          where p1' = stepPlayer roll p1

                memoisedOver :: Ix a => (a -> b) -> (a, a) -> a -> b
                f `memoisedOver` domain = (!) $ listArray domain $ map f $ range domain

                playerDomain = (join (,) (Player 1 1), join (,) (Player 21 10))

                tripleRolls :: [(Int, Integer)]
                tripleRolls = Map.toList $ frequencies $ add3 <$> die <*> die <*> die
                  where die = [1,2,3]
                        add3 x y z = x + y + z

        setRem :: Int -> Player -> Player
        setRem x (Player _ p) = Player x p

        stepPlayer :: Int -> Player -> Player
        stepPlayer n (Player r p) = Player (r - p') p'
          where p' = mod1 (p + n) 10
