import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.ParserCombinators.ReadP as RP
import Bits

data Reg = W | X | Y | Z deriving (Ord, Eq, Enum, Ix)
data Ph = RegPh Reg | ValPh Int deriving (Eq)
data MathOp = Add | Mul | Div | Mod | Eql deriving (Ord, Eq, Enum)
data Insn = Inp Reg | Math MathOp Reg Ph deriving (Eq)

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

instance Read Ph where
  readsPrec _ = RP.readP_to_S $
    (RegPh <$> RP.readS_to_P reads) RP.<++
    (ValPh <$> RP.readS_to_P reads)
instance Show Ph where
  show (RegPh reg) = show reg
  show (ValPh val) = show val

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

instance Show Insn where
  show (Inp reg) = "inp " ++ show reg
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
  . swap . extremes . gatherConstraintsFromTys
  where gatherConstraintsFromTys :: [Insn] -> [Constraint]
        gatherConstraintsFromTys insns = snd $ inferTys insns -- see below

        gatherConstraints :: [Insn] -> [Constraint]
        gatherConstraints insns = constraints
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
                  -- if we are popping, we have to make sure we don't push after, so
                  -- in[i]
                  -- must be equal to the top value of the stack, plus a, which is
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

data Ty
  = TyVar Int Integer -- var + offset
  | TyConst Integer
  | TyPoly26 [Ty] -- a + b 26 + c 26^2 + ... + z 26^n
  | TyCmp
    { aTy :: Ty
    , bTy :: Ty
    , eqvTy :: Ty
    , neqTy :: Ty }
  | Poison
  deriving (Eq, Show)

inferTys :: [Insn] -> (Ty, [Constraint])
inferTys insns = run3
  where startRegs = listArray (W,Z) (replicate 4 $ TyConst 0)
        startConstraints = []
        run1 = evalStateT (inferAll insns) 0
        run2 = evalStateT run1 startRegs
        run3 = runState run2 startConstraints

        inferAll :: [Insn] -> StateT Int (StateT (Array Reg Ty) (State [Constraint])) Ty
        inferAll insns = do
          mapM_ inferTyAndSet insns
          assertZTrue
          lift $ (! Z) <$> get

        inferTyAndSet insn = do
          (reg, ty) <- inferTy insn
          -- trace (show insn ++ " :: " ++ show ty) $ return ()
          lift $ modify (//[(reg, ty)])

        assertZTrue = do
          tys <- lift get
          zTy <- lift $ lift $ assertTruth (tys!Z)
          lift $ modify (//[(Z, zTy)])

        assertTruth :: Ty -> State [Constraint] Ty
        assertTruth (TyCmp (TyVar i a) (TyVar j b) c _) = do
          modify (Constraint i (fromInteger (a - b)) j :)
          return c
        assertTruth ty = return ty

        inferTy (Inp reg) = do
          i <- get
          -- trace (show (Inp reg) ++ " -> " ++ show i) $ return ()
          modify succ
          assertZTrue
          return (reg, TyVar i 0)
        inferTy (Math op reg arg) = do
          regTy <- lift $ (! reg) <$> get
          argTy <-
            case arg of
              RegPh r -> lift $ (! r) <$> get
              ValPh i -> return $ TyConst $ toInteger i
          if op == Eql
            then return (reg, tyCmp regTy argTy (TyConst 1) (TyConst 0))
            else return (reg, tyMath op regTy argTy)

tyCmp :: Ty -> Ty -> Ty -> Ty -> Ty
tyCmp (TyCmp a b c d) bTy eqvTy neqTy = tyCmp a b (tyCmp c bTy eqvTy neqTy) (tyCmp d bTy eqvTy neqTy)
tyCmp aTy (TyCmp a b c d) eqvTy neqTy = tyCmp a b (tyCmp aTy c eqvTy neqTy) (tyCmp aTy d eqvTy neqTy)
tyCmp aTy bTy (TyCmp a b c d) neqTy | aTy == a && bTy == b = tyCmp aTy bTy c neqTy
tyCmp aTy bTy eqvTy (TyCmp a b c d) | aTy == a && bTy == b = tyCmp aTy bTy eqvTy d
tyCmp _ _ eqvTy neqTy | eqvTy == neqTy = eqvTy
tyCmp (TyConst a) (TyConst b) eqvTy neqTy | a == b = eqvTy | otherwise = neqTy
tyCmp (TyConst k) (TyVar b i) eqvTy neqTy | not $ inRange (1,9) (k - i) = neqTy
tyCmp a@(TyVar _ _) b@(TyConst _) eqvTy neqTy = tyCmp b a eqvTy neqTy
tyCmp (TyVar i a) (TyVar j b) eqvTy neqTy
  | i == j && a == b = eqvTy
  | i == j = neqTy
  | abs (a - b) >= 9 = neqTy
tyCmp aTy bTy eqvTy neqTy = TyCmp aTy bTy eqvTy neqTy

tyMath :: MathOp -> Ty -> Ty -> Ty
tyMath _ Poison _ = Poison
tyMath _ _ Poison = Poison
tyMath Mul (TyConst 0) _ = TyConst 0
tyMath Mul _ (TyConst 0) = TyConst 0
tyMath Mul (TyConst 1) rhs = rhs
tyMath Mul lhs (TyConst 1) = lhs
tyMath Add (TyConst 0) rhs = rhs
tyMath Add lhs (TyConst 0) = lhs
tyMath Div ty (TyConst 1) = ty
tyMath op lhs (TyCmp a b eqvTy neqTy) = tyCmp a b (tyMath op lhs eqvTy) (tyMath op lhs neqTy)
tyMath op (TyCmp a b eqvTy neqTy) rhs = tyCmp a b (tyMath op eqvTy rhs) (tyMath op neqTy rhs)
tyMath op (TyConst a) (TyConst b) = TyConst $ mathOp op a b
tyMath Mul (TyConst 26) (TyPoly26 poly) = TyPoly26 $ TyConst 0 : poly
tyMath Mul (TyPoly26 poly) (TyConst 26) = TyPoly26 $ TyConst 0 : poly
tyMath Mul (TyConst 26) ty = TyPoly26 [TyConst 0, ty]
tyMath Mul ty (TyConst 26) = TyPoly26 [TyConst 0, ty]
tyMath Add (TyPoly26 (k:rest)) ty = TyPoly26 $ tyMath Add k ty : rest
tyMath Add (TyVar i a) (TyConst b) = TyVar i (a + b)
tyMath Add (TyConst b) (TyVar i a) = TyVar i (a + b)
tyMath Add (TyVar _ _) (TyVar _ _) = Poison
tyMath Div (TyPoly26 (_:rest)) (TyConst 26) | null rest = TyConst 0 | otherwise = TyPoly26 rest
tyMath Mod (TyPoly26 (v:_)) (TyConst 26) = v
tyMath Mod lhs@(TyVar i b) (TyConst 26) | b + 9 <= 25 = lhs
tyMath op lhs rhs = error $ "No op " ++ (show op) ++ " " ++ (show lhs) ++ " and " ++ (show rhs)

mathOp :: (Integral a) => MathOp -> a -> a -> a
mathOp Add = (+)
mathOp Mul = (*)
mathOp Div = div
mathOp Mod = mod
mathOp Eql = \ a b -> if a == b then 1 else 0
