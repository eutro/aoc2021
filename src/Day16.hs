import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
import Control.Monad.State
import Data.Array
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Data.Function
import Data.Ix
import Data.Tuple
import Debug.Trace
import Util

data Packet = Packet { version :: Int, payload :: Payload } deriving (Show)
data Payload = Literal Int | Operator Int [Packet] deriving (Show)

type RState a = State [Int] a

main :: IO ()
main = do
  input <- getLine
  let bits = concatMap hexDigitToBits input
      packet = evalState readPacket bits
  print $ verSum packet
  print $ evalPacket packet
  where hexDigitToBits = (Map.!)
                         (Map.fromList
                          [('0', [0,0,0,0]), ('1', [0,0,0,1]), ('2', [0,0,1,0]), ('3', [0,0,1,1]),
                           ('4', [0,1,0,0]), ('5', [0,1,0,1]), ('6', [0,1,1,0]), ('7', [0,1,1,1]),
                           ('8', [1,0,0,0]), ('9', [1,0,0,1]), ('A', [1,0,1,0]), ('B', [1,0,1,1]),
                           ('C', [1,1,0,0]), ('D', [1,1,0,1]), ('E', [1,1,1,0]), ('F', [1,1,1,1])])

        verSum (Packet ver (Literal _)) = ver
        verSum (Packet ver (Operator _ subs)) = ver + sum (map verSum subs)

        binOp op [a, b] = fromEnum $ a `op` b
        lookupOp = (Map.!)
                   (Map.fromList
                    [(0, sum),
                     (1, product),
                     (2, minimum),
                     (3, maximum),
                     (5, binOp (>)),
                     (6, binOp (<)),
                     (7, binOp (==))])

        evalPacket (Packet _ (Literal n)) = n
        evalPacket (Packet _ (Operator op subs)) = lookupOp op $ map evalPacket subs

        takeM :: Int -> RState [Int]
        takeM = state . splitAt
        headM :: RState Int
        headM = state (fromJust . uncons)

        readPacket :: RState Packet
        readPacket = do
          ver <- readBinDigits <$> takeM 3
          tag <- readBinDigits <$> takeM 3
          payload <- readPayload tag
          return $ Packet ver payload

        readPayload :: Int -> RState Payload
        readPayload 4 = Literal <$> readIntLEBn't
        readPayload op = Operator op <$> readOpPackets

        readIntLEBn't :: RState Int
        readIntLEBn't = do
          cont <- headM
          digit <- readBinDigits <$> takeM 4
          if toEnum cont
            then (2^4 * digit +) <$> readIntLEBn't
            else return digit

        readOpPackets :: RState [Packet]
        readOpPackets = do
          tag <- headM
          case tag of
            0 -> readBinDigits <$> takeM 15 >>= fmap readAllPackets . takeM
            1 -> readBinDigits <$> takeM 11 >>= readNPackets

        readAllPackets :: [Int] -> [Packet]
        readAllPackets [] = []
        readAllPackets ls = p : readAllPackets rest
          where (p, rest) = runState readPacket ls

        readNPackets :: Int -> RState [Packet]
        readNPackets 0 = return []
        readNPackets n = do
          packet <- readPacket
          (packet:) <$> readNPackets (pred n)
