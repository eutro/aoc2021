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

                unwonStates = takeWhile (not . winning) stateSeq
                  where winning (_, p2) = remaining p2 <= 0

                tripleRolls = map (sum . take 3) $ iterate (drop 3) singleRolls
                  where singleRolls = cycle [1..100]

                stateSeq = scanl stepState start tripleRolls
                  where stepState :: FoldFn (Player, Player) Int
                        stepState (p1, p2) roll = (p2, stepPlayer roll p1)

        quantumDie :: (Player, Player) -> (Integer, Integer)
        quantumDie =  compute `memoisedOver` playerDomain
          where compute (p1, p2) = foldl (zipPos (+)) (0, 0) $ do
                  (roll, freq) <- tripleRolls
                  let p1n = stepPlayer roll p1
                  return $ if remaining p1n <= 0
                    then (freq, 0)
                    else mapBoth (freq*) $ swap $ quantumDie (p2, p1n)

                memoisedOver :: Ix a => (a -> b) -> (a, a) -> a -> b
                f `memoisedOver` domain = (!) $ listArray domain $ map f $ range domain

                playerDomain = ((minPlayer, minPlayer), (maxPlayer, maxPlayer))
                  where minPlayer = Player 1 1
                        maxPlayer = Player 21 10

                tripleRolls :: [(Int, Integer)]
                tripleRolls = Map.toList $ frequencies $ add3 <$> die <*> die <*> die
                  where die = [1,2,3]
                        add3 x y z = x + y + z
