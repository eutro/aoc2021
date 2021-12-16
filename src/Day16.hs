import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Control.Monad
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

hexDigitToBits :: Char -> [Int]
hexDigitToBits '0' = [0,0,0,0]
hexDigitToBits '1' = [0,0,0,1]
hexDigitToBits '2' = [0,0,1,0]
hexDigitToBits '3' = [0,0,1,1]
hexDigitToBits '4' = [0,1,0,0]
hexDigitToBits '5' = [0,1,0,1]
hexDigitToBits '6' = [0,1,1,0]
hexDigitToBits '7' = [0,1,1,1]
hexDigitToBits '8' = [1,0,0,0]
hexDigitToBits '9' = [1,0,0,1]
hexDigitToBits 'A' = [1,0,1,0]
hexDigitToBits 'B' = [1,0,1,1]
hexDigitToBits 'C' = [1,1,0,0]
hexDigitToBits 'D' = [1,1,0,1]
hexDigitToBits 'E' = [1,1,1,0]
hexDigitToBits 'F' = [1,1,1,1]

data Packet = Packet { version :: Int, payload :: Payload } deriving (Show)
data Payload = Literal Int | Operator Int [Packet] deriving (Show)

main :: IO ()
main = do
  input <- getLine
  let bits = concatMap hexDigitToBits input
      (packet, _) = readPacket bits
  print $ verSum packet
  print $ evalPacket packet
  return ()
  where readPacket (v0:v1:v2:t0:t1:t2:tail) =
          (Packet (readBinDigits [v0,v1,v2]) payload, rest)
          where (payload, rest) = readPacket' (readBinDigits [t0,t1,t2]) tail
        readPackets [] = []
        readPackets ls = p : readPackets rest where (p, rest) = readPacket ls
        readNPackets 0 bits = ([], bits)
        readNPackets n bits = (p : rp, rest') where (p, rest) = readPacket bits
                                                    (rp, rest') = readNPackets (pred n) rest
        readPacket' 4 payload = (Literal $ readBinDigits value, rest) where (value, rest) = readIntLEBn't payload
        readPacket' op payload = (Operator op subs, rest) where (subs, rest) = readOpPackets payload
        readOpPackets (0:tail) = (readPackets subs, rest')
          where (len, rest) = mapP readBinDigits id $ splitAt 15 tail
                (subs, rest') = splitAt len rest
        readOpPackets (1:tail) = (subs, rest')
          where (count, rest) = mapP readBinDigits id $ splitAt 11 tail
                (subs, rest') = readNPackets count rest

        readIntLEBn't (1:b0:b1:b2:b3:tail) = ([b0,b1,b2,b3] ++ rest, unread) where (rest, unread) = readIntLEBn't tail
        readIntLEBn't (0:b0:b1:b2:b3:tail) = ([b0,b1,b2,b3], tail)

        verSum (Packet ver (Literal _)) = ver
        verSum (Packet ver (Operator _ subs)) = ver + sum (map verSum subs)

        evalPacket (Packet _ (Literal n)) = n
        evalPacket (Packet _ (Operator op subs)) =
          (case op of
             0 -> sum; 1 -> product; 2 -> minimum; 3 -> maximum
             5 -> fromEnum . uncurry (>) . listToP
             6 -> fromEnum . uncurry (<) . listToP
             7 -> fromEnum . uncurry (==) . listToP) $ map evalPacket subs
