module TigerAssem where

import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )
import           TigerSymbol
import           TigerTemp                     as Temp
import           Control.Monad.State
import           TigerTree                     as Tree
import           TigerUnique
import           Data.Int
import           TigerFrame
import           TigerAssemTypes
import           Data.Bits
import           Data.List                     as L
import           Text.PrettyPrint
import           Data.Char                      ( toLower )
import           Debug.Trace                    ( trace )

maxHalf :: Int
maxHalf = 2 ^ 15 - 1

minHalf :: Int
minHalf = -1 * 2 ^ 15

inHalfRange :: Int -> Bool
inHalfRange i = (i <= maxHalf) && (i >= minHalf)

isPower :: Int -> Maybe Int
isPower x = isPower' x 0
 where
  isPower' :: Int -> Int -> Maybe Int
  isPower' x i | testBit x i && clearBit x i == 0 = Just i
               | otherwise = if i == 31 then Nothing else isPower' x (i + 1)


type AssemAcc = StateT [Instr] StGen
emptyAcc = []

runMunch :: [Stm] -> StGen [Instr]
runMunch []           = return []
runMunch (stm : stms) = do
  (_, instrs) <- runStateT (munchStm stm) emptyAcc
  rest        <- runMunch stms
  return $ instrs ++ rest

emit :: Instr -> AssemAcc ()
emit i = modify (\s -> s ++ [i])


munchStm :: Stm -> AssemAcc ()
-- SEQ
munchStm (Seq  a b) = munchStm a >> munchStm b
-- JUMP
munchStm (Jump _ l) = do
  emit OPER {assem = J l, dsts = [], srcs = [], jump = Just [l]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
-- LABEL
munchStm (Label l                   ) = emit LABEL {assem = Lbl l, lab = l}
-- MOVE
munchStm (Move (Temp t) c@(Call _ _)) = do
  munchStm (ExpS c)
  emit MOV {assem = MOVE t rv, dst = t, src = rv}
munchStm (Move (Temp t) e2) = do -- Mas casos
  t2 <- munchExp e2
  emit MOV {assem = MOVE t t2, dst = t, src = t2}
munchStm (Move (Mem e) e2) = do
  d <- munchExp e
  t <- munchExp e2
  emit OPER {assem = SW t 0 d, dsts = [], srcs = [t, d], jump = Nothing}
-- EXPS
munchStm (ExpS (Call (Name l) args)) = do
  args' <- munchArgs 0 args
  emit OPER
    { assem = JAL l
    , dsts  = calldefs ++ argregs
    , srcs  = args'
    , jump  = Nothing
    }
  emit OPER
    { assem = NOP
    , dsts  = []
    , srcs  = callersaves ++ argregs
    , jump  = Nothing
    }
  restoreArgs
munchStm (ExpS c@(Call _ _)          ) = void $ munchExp c
munchStm (ExpS e@(Eseq _ _)          ) = void $ munchExp e
munchStm (ExpS _                     ) = return ()
-- CJUMP EQ
munchStm (CJump EQ (Const 0) e2 lf lt) = do
  t <- munchExp e2
  emit OPER {assem = BEQZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump EQ e1 (Const 0) lf lt) = do
  t <- munchExp e1
  emit OPER {assem = BEQZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump EQ e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BEQ t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
-- CJUMP NE
munchStm (CJump NE (Const 0) e2 lf lt) = do
  t <- munchExp e2
  emit OPER {assem = BNEZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump NE e1 (Const 0) lf lt) = do
  t <- munchExp e1
  emit OPER {assem = BNEZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump NE e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BNE t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
-- CJUMP LT
munchStm (CJump LT e1 (Const 0) lf lt) = do
  t <- munchExp e1
  emit OPER {assem = BLTZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump LT (Const 0) e2 lf lt) = do
  t <- munchExp e2
  emit OPER {assem = BGTZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump LT e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BLT t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump ULT e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BLTU t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
-- CJUMP LE
munchStm (CJump LE e1 (Const 0) lf lt) = do
  t <- munchExp e1
  emit OPER {assem = BLEZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump LE (Const 0) e2 lf lt) = do
  t <- munchExp e2
  emit OPER {assem = BGEZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump LE e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BLE t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump ULE e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BLEU t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
-- CJUMP GT
munchStm (CJump GT e1 (Const 0) lf lt) = do
  t <- munchExp e1
  emit OPER {assem = BGTZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump GT (Const 0) e2 lf lt) = do
  t <- munchExp e2
  emit OPER {assem = BLTZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump GT e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BGT t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump UGT e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BGTU t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
-- CJUMP GE
munchStm (CJump GE e1 (Const 0) lf lt) = do
  t <- munchExp e1
  emit OPER {assem = BGEZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump GE (Const 0) e2 lf lt) = do
  t <- munchExp e2
  emit OPER {assem = BLEZ t lf, srcs = [t], dsts = [], jump = Just [lf, lt]}
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump GE e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BGE t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
munchStm (CJump UGE e1 e2 lf lt) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER
    { assem = BGEU t1 t2 lf
    , srcs  = [t1, t2]
    , dsts  = []
    , jump  = Just [lf, lt]
    }
  emit OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}


munchArgs :: Int -> [Exp] -> AssemAcc [Temp]
munchArgs _ [] = return []
munchArgs n (e : es)
  | n <= 3 = do
    t <- munchExp e
    let a = argregs !! n
    emit MOV {assem = MOVE a t, src = t, dst = a}
    emit OPER
      { assem = SW a (n * wSz) sp
      , srcs  = [a, sp]
      , dsts  = []
      , jump  = Nothing
      }
    es' <- munchArgs (n + 1) es
    return $ a : es'
  | otherwise = do
    t <- munchExp e
    emit OPER
      { assem = SW t (n * wSz) sp
      , srcs  = [t, sp]
      , dsts  = []
      , jump  = Nothing
      }
    munchArgs (n + 1) es

restoreArgs :: AssemAcc ()
restoreArgs = mapM_
  (\(a, n) -> emit OPER
    { assem = LW a fp (n * wSz)
    , srcs  = [sp]
    , dsts  = [a]
    , jump  = Nothing
    }
  )
  (L.zip argregs [0 ..])

munchExp :: Exp -> AssemAcc Temp
-- CONST
munchExp (Const 0) = return zero
munchExp (Const i) = do
  let tr_i = trunc i
  t <- newTemp
  emit OPER {assem = LI t tr_i, srcs = [], dsts = [t], jump = Nothing}
  return t
-- NAME
munchExp (Name l) = do
  t <- newTemp
  emit OPER {assem = LA t l, srcs = [], dsts = [t], jump = Nothing}
  return t
-- TEMP
munchExp (Temp t        ) = return t
-- MEM
munchExp (Mem  (Const i)) = do
  let tr_i = trunc i
  t <- newTemp
  emit OPER {assem = LI t tr_i, srcs = [], dsts = [t], jump = Nothing}
  return t
-- MEM SUMA
munchExp (Mem p@(Binop Plus (Const i) e))
  | inHalfRange i = do
    d <- newTemp
    s <- munchExp e
    emit OPER {assem = LW d s i, srcs = [s], dsts = [d], jump = Nothing}
    return d
  | otherwise = genericMem p
munchExp (Mem p@(Binop Plus e (Const i)))
  | inHalfRange i = do
    d <- newTemp
    s <- munchExp e
    emit OPER {assem = LW d s i, srcs = [s], dsts = [d], jump = Nothing}
    return d
  | otherwise = genericMem p
munchExp (Mem e     ) = genericMem e
-- ESEQ
munchExp (Eseq stm e) = do
  munchStm stm
  munchExp e
-- SUMA
munchExp (Binop Plus (Const 0) e        ) = munchExp e
munchExp (Binop Plus e         (Const 0)) = munchExp e
munchExp (Binop Plus e1 (Const i))
  | inHalfRange i = do
    t <- newTemp
    s <- munchExp e1
    emit OPER {assem = ADDI t s i, srcs = [s], dsts = [t], jump = Nothing}
    return t
  | otherwise = genericAdd e1 (Const i)
munchExp (Binop Plus (Const i) e1)
  | inHalfRange i = do
    t <- newTemp
    s <- munchExp e1
    emit OPER {assem = ADDI t s i, srcs = [s], dsts = [t], jump = Nothing}
    return t
  | otherwise = genericAdd (Const i) e1
munchExp (Binop Plus  e1 e2       ) = genericAdd e1 e2
-- RESTA
munchExp (Binop Minus e  (Const 0)) = munchExp e
munchExp (Binop Minus e1 (Const i))
  | inHalfRange (-i) = do
    t <- newTemp
    s <- munchExp e1
    emit OPER {assem = ADDI t s (-i), srcs = [s], dsts = [t], jump = Nothing}
    return t
  | otherwise = genericSub e1 (Const i)
munchExp (Binop Minus e1        e2) = genericSub e1 e2
-- MULT
munchExp (Binop Mul   (Const i) e2) = case isPower i of
  Just n -> do
    t <- newTemp
    s <- munchExp e2
    emit OPER {assem = SLL t s n, dsts = [t], srcs = [s], jump = Nothing}
    return t
  Nothing -> genericMult (Const i) e2
munchExp (Binop Mul e1 (Const i)) = case isPower i of
  Just n -> do
    t <- newTemp
    s <- munchExp e1
    emit OPER {assem = SLL t s n, dsts = [t], srcs = [s], jump = Nothing}
    return t
  Nothing -> genericMult e1 (Const i)
munchExp (Binop Mul e1 e2) = genericMult e1 e2
-- DIV
munchExp (Binop Div e1 e2) = genericDiv e1 e2
-- AND
munchExp (Binop And (Const i) e)
  | inHalfRange i = do
    d <- newTemp
    s <- munchExp e
    emit OPER {assem = ANDI d s i, srcs = [s], dsts = [d], jump = Nothing}
    return d
  | otherwise = genericAND (Const i) e
munchExp (Binop And e (Const i))
  | inHalfRange i = do
    d <- newTemp
    s <- munchExp e
    emit OPER {assem = ANDI d s i, srcs = [s], dsts = [d], jump = Nothing}
    return d
  | otherwise = genericAND e (Const i)
munchExp (Binop And e1 e2) = genericAND e1 e2
-- OR
munchExp (Binop Or  e1 e2) = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = OR d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d
--XOR
munchExp (Binop XOr (Const i) e)
  | inHalfRange i = do
    d <- newTemp
    s <- munchExp e
    emit OPER {assem = XORI d s i, srcs = [s], dsts = [d], jump = Nothing}
    return d
  | otherwise = genericXOR (Const i) e
munchExp (Binop XOr e (Const i))
  | inHalfRange i = do
    d <- newTemp
    s <- munchExp e
    emit OPER {assem = XORI d s i, srcs = [s], dsts = [d], jump = Nothing}
    return d
  | otherwise = genericXOR e (Const i)
munchExp (Binop XOr    e1 e2) = genericXOR e1 e2
-- Shifts
munchExp (Binop LShift e1 e2) = do
  d  <- newTemp
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER {assem = SLLV d t1 t2, srcs = [t1, t2], dsts = [d], jump = Nothing}
  return d
munchExp (Binop RShift e1 e2) = do
  d  <- newTemp
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER {assem = SRLV d t1 t2, srcs = [t1, t2], dsts = [d], jump = Nothing}
  return d
munchExp (Binop ARShift e1 e2) = do
  d  <- newTemp
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit OPER {assem = SRAV d t1 t2, srcs = [t1, t2], dsts = [d], jump = Nothing}
  return d
-- CALL
munchExp c@(Call _ _) = error $ "Call no tendria que estar aca: " ++ show c



genericMem :: Exp -> AssemAcc Temp
genericMem e = do
  d <- newTemp
  s <- munchExp e
  emit OPER {assem = LW d s 0, srcs = [s], dsts = [d], jump = Nothing}
  return d

genericAdd :: Exp -> Exp -> AssemAcc Temp
genericAdd e1 e2 = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = ADD d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d

genericSub :: Exp -> Exp -> AssemAcc Temp
genericSub e1 e2 = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = SUB d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d

genericMult :: Exp -> Exp -> AssemAcc Temp
genericMult e1 e2 = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = MUL d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d

genericDiv :: Exp -> Exp -> AssemAcc Temp
genericDiv e1 e2 = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = DIV d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d

genericAND :: Exp -> Exp -> AssemAcc Temp
genericAND e1 e2 = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = AND d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d

genericXOR :: Exp -> Exp -> AssemAcc Temp
genericXOR e1 e2 = do
  d <- newTemp
  s <- munchExp e1
  t <- munchExp e2
  emit OPER {assem = XOR d s t, srcs = [s, t], dsts = [d], jump = Nothing}
  return d

getTemps :: Instr -> [Temp]
getTemps (OPER _ dsts srcs _) = L.union dsts srcs
getTemps (LABEL _ _         ) = []
getTemps (MOV _ d s         ) = L.union [d] [s]

getTempsL :: [Instr] -> [Temp]
getTempsL = L.foldr (L.union . getTemps) []

-- Non machine temps
getTempsLNM :: [Instr] -> [Temp]
getTempsLNM = L.filter temp . getTempsL
 where
  temp :: Temp -> Bool
  temp = (== 'T') . L.head . unpack

trunc :: Int -> Int
trunc i = fromIntegral (fromIntegral (i :: Int) :: Int32) :: Int

-----------------------------------------------------------------
-- Printing
-----------------------------------------------------------------
renderStrFrag :: Frag -> String
renderStrFrag s@(AString l ts) =
  let dirs = concatMap ((++ "\n") . renderDirective)
                       [GLOBL l, DATA, ALIGN 2, SIZE l 4]
  in  dirs ++ show s

renderInstr :: Instr -> String
renderInstr (DIR d) = renderDirective d
renderInstr i       = renderMIPSAssem $ assem i

renderInstrs :: [Instr] -> String
renderInstrs = concatMap (\i -> renderInstr i ++ "\n")

pDirective :: MIPSDir -> Doc
pDirective (GLOBL l ) = tab <> text ".globl" <+> pLabel l
pDirective (ALIGN n ) = tab <> text ".align" <+> int n
pDirective (SIZE l n) = tab <> text ".size" <+> pLabel l <> comma <+> int n
pDirective (FRAME t1 n t2) =
  tab <> text ".frame" <+> pTemp t1 <> comma <+> int n <> comma <+> pTemp t2
pDirective d = tab <> text "." <> text (toLower <$> show d)

renderDirective :: MIPSDir -> String
renderDirective = render . pDirective

tab = space <> space

pTemp :: Temp -> Doc
pTemp t = text $ "$" ++ unpack t

pLabel :: Label -> Doc
pLabel l = text $ unpack l

(<.>) :: Doc -> Doc -> Doc
d1 <.> d2 = d1 <> comma <> d2

isTemp :: Label -> Bool
isTemp n = case unpack n of
  ('T' : _) -> True
  _ | L.elem n machineregs -> True
    | otherwise            -> False

pMIPSAssem :: MIPSAssem -> Doc
pMIPSAssem (J n) | isTemp n  = tab <> text "j" <+> pTemp n
                 | otherwise = tab <> text "j" <+> pLabel n
pMIPSAssem (Lbl l     ) = pLabel l <> colon
pMIPSAssem (MOVE t1 t2) = tab <> text "move" <+> pTemp t1 <.> pTemp t2
pMIPSAssem (SW t1 i t2) =
  tab <> text "sw" <+> pTemp t1 <.> int i <> lparen <> pTemp t2 <> rparen
pMIPSAssem (LW t1 t2 i) =
  tab <> text "lw" <+> pTemp t1 <.> int i <> lparen <> pTemp t2 <> rparen
pMIPSAssem (LI rd v) = tab <> text "li" <+> pTemp rd <.> int v
pMIPSAssem (LA rd l) = tab <> text "la" <+> pTemp rd <.> pLabel l
pMIPSAssem (ADDI rd rs i) =
  tab <> text "addi" <+> pTemp rd <.> pTemp rs <.> int i
pMIPSAssem (ADD rd rs rt) =
  tab <> text "add" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (SUB rd rs rt) =
  tab <> text "sub" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (MUL rd rs rt) =
  tab <> text "mul" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (DIV rd rs rt) =
  tab <> text "div" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (AND rd rs rt) =
  tab <> text "and" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (ANDI rd rs i) =
  tab <> text "andi" <+> pTemp rd <.> pTemp rs <.> int i
pMIPSAssem (OR rd rs rt) =
  tab <> text "or" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (ORI rd rs i) =
  tab <> text "ori" <+> pTemp rd <.> pTemp rs <.> int i
pMIPSAssem (XOR rd rs rt) =
  tab <> text "xor" <+> pTemp rd <.> pTemp rs <.> pTemp rt
pMIPSAssem (XORI rd rs i) =
  tab <> text "xori" <+> pTemp rd <.> pTemp rs <.> int i
pMIPSAssem (SLL rd rt sa) =
  tab <> text "sll" <+> pTemp rd <.> pTemp rt <.> int sa
pMIPSAssem (SRL rd rt sa) =
  tab <> text "srl" <+> pTemp rd <.> pTemp rt <.> int sa
pMIPSAssem (SRA rd rt sa) =
  tab <> text "sra" <+> pTemp rd <.> pTemp rt <.> int sa
pMIPSAssem (SLLV rd rt rs) =
  tab <> text "sllv" <+> pTemp rd <.> pTemp rt <.> pTemp rs
pMIPSAssem (SRLV rd rt rs) =
  tab <> text "srlv" <+> pTemp rd <.> pTemp rt <.> pTemp rs
pMIPSAssem (SRAV rd rt rs) =
  tab <> text "srav" <+> pTemp rd <.> pTemp rt <.> pTemp rs
pMIPSAssem (BEQ rs rt l) =
  tab <> text "beq" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BNE rs rt l) =
  tab <> text "bne" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BLT rs rt l) =
  tab <> text "blt" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BLTU rs rt l) =
  tab <> text "bltu" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BLE rs rt l) =
  tab <> text "ble" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BLEU rs rt l) =
  tab <> text "bleu" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BGT rs rt l) =
  tab <> text "bgt" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BGTU rs rt l) =
  tab <> text "bgtu" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BGE rs rt l) =
  tab <> text "bge" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BGEU rs rt l) =
  tab <> text "bgeu" <+> pTemp rs <.> pTemp rt <.> pLabel l
pMIPSAssem (BLTZ rs l) = tab <> text "bltz" <+> pTemp rs <.> pLabel l
pMIPSAssem (BLEZ rs l) = tab <> text "blez" <+> pTemp rs <.> pLabel l
pMIPSAssem (BGTZ rs l) = tab <> text "bgtz" <+> pTemp rs <.> pLabel l
pMIPSAssem (BGEZ rs l) = tab <> text "bgez" <+> pTemp rs <.> pLabel l
pMIPSAssem (BEQZ rs l) = tab <> text "beqz" <+> pTemp rs <.> pLabel l
pMIPSAssem (BNEZ rs l) = tab <> text "bnez" <+> pTemp rs <.> pLabel l
pMIPSAssem (JAL l    ) = tab <> text "jal" <+> pLabel l
pMIPSAssem NOP         = tab <> text "nop"
pMIPSAssem Empty       = Text.PrettyPrint.empty

pMIPSAssems :: [MIPSAssem] -> Doc
pMIPSAssems code = vcat (map pMIPSAssem code)

renderMIPSAssem :: MIPSAssem -> String
renderMIPSAssem = render . pMIPSAssem

renderMIPSAssems :: [MIPSAssem] -> String
renderMIPSAssems = render . pMIPSAssems
