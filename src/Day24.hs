import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as RP
import Bits

data Reg = W | X | Y | Z deriving (Ord, Eq, Enum, Ix, Show)
data Ph = RegPh Reg | ValPh Int deriving (Show, Eq)
data MathOp = Add | Mul | Div | Mod | Eql deriving (Ord, Eq, Enum, Show)
data Insn =
  Inp Reg |
  Math MathOp Reg Ph
  deriving (Eq, Show)

data Constraint = Constraint
  {inA::Int, diff::Int, inB::Int}
  deriving (Show)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  let (mm, mM) = extremeConstraints $ insnsToConstraints input
  putStrLn $ map intToDigit mM
  putStrLn $ map intToDigit mm
  return ()
  where parseInput str = insns
          where insns = map readInsn
                  $ filter (not . isComment)
                  $ takeWhile (/="## EOF")
                  $ lines str
                isComment [] = True
                isComment ('#':_) = True
                isComment _ = False
                parseReg "w" = W
                parseReg "x" = X
                parseReg "y" = Y
                parseReg "z" = Z
                readInsn str = parseParts parts
                  where parts = splitOn " " str
                        parseOp = (Map.!)
                                  (Map.fromList $ zip
                                   ["add", "mul", "div", "mod", "eql"]
                                   [Add, Mul, Div, Mod, Eql])
                        parseParts ["inp", reg] = Inp (parseReg reg)
                        parseParts [op, reg, ph] = Math (parseOp op) (parseReg reg) (parsePh ph)
                        parsePh ph = case reads ph of
                                       [(int, _)] -> ValPh int
                                       _ -> RegPh $ parseReg ph

        insnsToConstraints :: [Insn] -> [Constraint]
        insnsToConstraints insns = constraints
          where subPrograms = splitOn [Inp W] insns
                pairs = sequence $ zipWith compileSubprogram [0..] subPrograms
                constraints = execState (evalStateT pairs []) []
                compileSubprogram :: Int -> [Insn] -> (StateT [(Int, Int)]) (State [Constraint]) ()
                compileSubprogram i
                  [Math Mul X (ValPh 0)
                  ,Math Add X (RegPh Z)
                  ,Math Mod X (ValPh 26)
                  ,Math Div Z (ValPh maybePop)
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
                compileSubprogram _ insns = error $ "huh? " ++ show insns

        maximiseDiff diff
          | diff < 0 = (9, 9 + diff)
          | otherwise = (9 - diff, 9)

        minimiseDiff diff
          | diff < 0 = (1 - diff, 1)
          | otherwise = (1, 1 + diff)

        extremeConstraints :: [Constraint] -> ([Int], [Int])
        extremeConstraints cnstrs = unzip $ map snd $ sortBy (compare `on` fst) $ do
          Constraint i diff j <- cnstrs
          let (im, jm) = minimiseDiff diff
          let (iM, jM) = maximiseDiff diff
          [(i, (im, iM)), (j, (jm, jM))]
