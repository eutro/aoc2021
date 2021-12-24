import qualified Data.Map as Map
import qualified Text.ParserCombinators.ReadP as RP
import Bits

data Reg = W | X | Y | Z deriving (Ord, Eq, Enum, Ix)
instance Read Reg where
  readsPrec _ ('w':r) = [(W, r)]
  readsPrec _ ('x':r) = [(X, r)]
  readsPrec _ ('y':r) = [(Y, r)]
  readsPrec _ ('z':r) = [(Z, r)]
  readsPrec _ _ = []
instance Show Reg where
  show W = "w"
  show X = "x"
  show Y = "y"
  show Z = "z"

data Ph = RegPh Reg | ValPh Int deriving (Eq)
instance Read Ph where
  readsPrec _ = RP.readP_to_S $
    (RegPh <$> RP.readS_to_P reads) RP.<++
    (ValPh <$> RP.readS_to_P reads)
instance Show Ph where
  show (RegPh reg) = show reg
  show (ValPh val) = show val

data MathOp = Add | Mul | Div | Mod | Eql deriving (Ord, Eq, Enum)
instance Show MathOp where
  show Add = "add"
  show Mul = "mul"
  show Div = "div"
  show Mod = "mod"
  show Eql = "eql"
instance Read MathOp where
  readsPrec _ ('a':'d':'d':r) = [(Add, r)]
  readsPrec _ ('m':'u':'l':r) = [(Mul, r)]
  readsPrec _ ('d':'i':'v':r) = [(Div, r)]
  readsPrec _ ('m':'o':'d':r) = [(Mod, r)]
  readsPrec _ ('e':'q':'l':r) = [(Eql, r)]
  readsPrec _ _ = []

data Insn = Inp Reg | Math MathOp Reg Ph deriving (Eq)
instance Show Insn where
  show (Inp reg) = show reg
  show (Math op reg ph) = show op ++ " " ++ show reg ++ " " ++ show ph
instance Read Insn where
  readsPrec _ = RP.readP_to_S $
    (fmap Inp $ RP.string "inp " >> RP.readS_to_P reads) RP.<++
    (Math
     <$> (RP.readS_to_P reads <* RP.char ' ')
     <*> (RP.readS_to_P reads <* RP.char ' ')
     <*> (RP.readS_to_P reads))

data Constraint = Constraint {inA::Int, diff::Int, inB::Int} deriving (Show)

main :: IO ()
main = map read <$> lines <$> getContents
  >>= uncurry (on (>>) (putStrLn . map intToDigit))
  . swap . extremes . insnsToConstraints
  where insnsToConstraints :: [Insn] -> [Constraint]
        insnsToConstraints insns = constraints
          where subPrograms = splitOn [Inp W] insns
                constraintsM = sequence $ zipWith compileSubprogram [0..] subPrograms
                constraints = execState (evalStateT constraintsM []) []
                compileSubprogram :: Int -> [Insn] -> (StateT [(Int, Int)]) (State [Constraint]) ()
                compileSubprogram i
                  [Math Mul X (ValPh 0)
                  ,Math Add X (RegPh Z)
                  ,Math Mod X (ValPh 26)
                  ,Math Div Z (ValPh maybePop) -- 1 or 26, 50% of the time each
                  ,Math Add X (ValPh offA)
                  ,Math Eql X (RegPh W)
                  ,Math Eql X (ValPh 0) -- x = (z % 26 + offA) != w
                  ,Math Mul Y (ValPh 0)
                  ,Math Add Y (ValPh 25)
                  ,Math Mul Y (RegPh X)
                  ,Math Add Y (ValPh 1)
                  ,Math Mul Z (RegPh Y)
                  ,Math Mul Y (ValPh 0)
                  ,Math Add Y (RegPh W)
                  ,Math Add Y (ValPh offB)
                  ,Math Mul Y (RegPh X)
                  ,Math Add Z (RegPh Y) -- if x { z *= 26; z += w + offB; }
                  ]
                  -- if we're not popping, we're pushing
                  -- in[i] + offB
                  -- onto the "stack" z
                  | maybePop == 1 && offA >= 9 = modify ((i, offB):)
                  -- if we are popping, we have to make sure
                  -- we don't push after, so
                  -- in[i]
                  -- must be equal to the top value of the stack, which is
                  -- in[j] + b + a
                  -- therefore
                  -- in[j] + a + b = in[i]
                  | maybePop == 26 = do
                      (j, ob) <- state $ fromJust . uncons
                      lift $ modify (Constraint j (ob + offA) i :)

        extremes :: [Constraint] -> ([Int], [Int])
        extremes cnstrs = unzip $ map snd $ sortBy (compare `on` fst) $ do
          Constraint i diff j <- cnstrs
          let (im, jm)
                | diff < 0 = (9, 9 + diff)
                | otherwise = (9 - diff, 9)
              (iM, jM)
                | diff < 0 = (1 - diff, 1)
                | otherwise = (1, 1 + diff)
          [(i, (im, iM)), (j, (jm, jM))]
