import qualified Data.Map as Map
import qualified Data.Set as Set
import Bits

data Player = Player {score::Int,pos::Int} deriving Show
data GameState = GS {die::[Int],rolls::Int,players::(Player, Player)} deriving Show
type RState = State GameState

mod1 :: Int -> Int -> Int
mod1 a b = succ $ pred a `mod` b

main :: IO ()
main = do
  game <- parseInput <$> getContents
  print $ head $ catMaybes $ evalState (sequence $ repeat stepGame) game
  return ()
  where parseInput :: String -> GameState
        parseInput input = GS (cycle [1..100]) 0 $ listToP $ do
          line <- lines input
          let pos = read $ drop (length "Player 1 starting position: ") line
          return $ Player 0 pos

        stepGame :: RState (Maybe Int)
        stepGame = do
          p1s <- stepPlayer PairLeft
          if p1s >= 1000
            then Just <$> mulScores PairRight
            else do
            p2s <- stepPlayer PairRight
            if p2s >= 1000
              then Just <$> mulScores PairLeft
              else return Nothing

        mulScores :: PairSide -> RState Int
        mulScores loser = do
          GS _ rolls players <- get
          return $ score (getP loser players) * rolls

        stepPlayer :: PairSide -> RState Int
        stepPlayer side = do
          roll <- rollThrice
          GS die rolls players <- get
          let updatePlayer (Player score pos) = Player (score+newPos) newPos
                where newPos = (pos + roll) `mod1` 10
              np = updateP updatePlayer side players
          put $ GS die rolls np
          return $ score $ getP side np

        rollThrice :: RState Int
        rollThrice = sum <$> replicateM 3 rollOnce

        rollOnce :: RState Int
        rollOnce = do
          GS (die:restDie) rolls players <- get
          put $ GS restDie (succ rolls) players
          return die
