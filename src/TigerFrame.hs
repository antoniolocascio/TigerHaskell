module TigerFrame where

import           TigerTemp
import           TigerTree
import           TigerAbs                       ( Escapa(..) )
import           TigerSymbol
import           TigerAssemTypes
import           Debug.Trace
import qualified Data.List                     as L

import           Prelude                 hiding ( exp )

--

-- | Zero register
zero :: Temp
zero = pack "zero"

-- | Frame pointer
fp :: Temp
fp = pack "fp"

-- | Stack pointer
sp :: Temp
sp = pack "sp"

-- | Return values
rv = v0
v0 = pack "v0"
v1 = pack "v1"

-- | Return address
ra = pack "ra"

-- | Argumentos
a0 = pack "a0"
a1 = pack "a1"
a2 = pack "a2"
a3 = pack "a3"

-- | Saved temporaries (callee-saved)
s0 = pack "s0"
s1 = pack "s1"
s2 = pack "s2"
s3 = pack "s3"
s4 = pack "s4"
s5 = pack "s5"
s6 = pack "s6"
s7 = pack "s7"

-- | Temporaries (caller-saved)
t0 = pack "t0"
t1 = pack "t1"
t2 = pack "t2"
t3 = pack "t3"
t4 = pack "t4"
t5 = pack "t5"
t6 = pack "t6"
t7 = pack "t7"
t8 = pack "t8"
t9 = pack "t9"

-- | Word size in bytes
wSz :: Int
wSz = 4
-- | Base two logarithm of word size in bytes
log2WSz :: Int
log2WSz = 2

-- Estos offsets se utilizan para el calculo de acceso de variables que escapan
-- (principalmente)
-- | Offset
fpPrev :: Int
fpPrev = 0
-- | Donde se encuentra el FP del nivel anterior (no necesariamente el llamante?)
fpPrevLev :: Int
fpPrevLev = 0

-- | Esto es un offset previo a al lugar donde se encuentra el lugar de las variables
-- o de los argumentos.
argsGap, localsGap :: Int
argsGap = 0
localsGap = 4

-- | Dan inicio a los contadores de argumentos, variables y registros usados.
-- Ver |defaultFrame|
argsInicial, regInicial, localsInicial :: Int
argsInicial = 1
regInicial = 1
localsInicial = 0
paramsInicial = 4

specialregs = [v0, fp, sp, zero, ra]
argregs = [a0, a1, a2, a3]
calleesaves = [s0, s1, s2, s3, s4, s5, s6, s7]
callersaves = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, v1]
calldefs = v0 : callersaves
machineregs = specialregs ++ argregs ++ calleesaves ++ callersaves

data Access = InFrame Int | InReg Temp
    deriving Show
data Frag = Proc Stm Frame | AString Label [Symbol]

sepFrag :: [Frag] -> ([Frag], [(Stm, Frame)])
sepFrag xs = (reverse ass, reverse stmss)
 where
  (ass, stmss) = foldl
    (\(lbls, stms) x -> case x of
      Proc st fr -> (lbls, (st, fr) : stms)
      AString{}  -> (x : lbls, stms)
    )
    ([], [])
    xs

convertNLs :: String -> String
convertNLs ""            = ""
convertNLs ('\n' : rest) = "\\012" ++ convertNLs rest
convertNLs (c    : rest) = c : convertNLs rest

instance Show Frag where
    show (Proc s f) = "Frame:" ++ show f ++ '\n': show s
    show (AString l ts) = unpack l ++ ":" ++ (foldr (\t ts -> ("\n  " ++ convertNLs (unpack t)) ++ ts) "" ts)
data Frame = Frame {
        name        :: Symbol,
        formals     :: [Bool],
        locals      :: [Bool],
        actualArg   :: Int,
        actualLocal :: Int,
        actualReg   :: Int,
        maxParams   :: Int
    }
    deriving Show

defaultFrame :: Frame
defaultFrame = Frame
  { name        = empty
  , formals     = []
  , locals      = []
  , actualArg   = argsInicial
  , actualLocal = localsInicial
  , actualReg   = regInicial
  , maxParams   = paramsInicial
  }

-- TODOS A stack por i386
prepFormals :: Frame -> [Access]
prepFormals fs = prepFormals' (formals fs) argsInicial 1

prepFormals' :: [Bool] -> Int -> Integer -> [Access]
prepFormals' []          _ _ = []
prepFormals' (True : bs) n r = InFrame n : prepFormals' bs (n + argsGap) r
prepFormals' (False : bs) n r =
  InReg (detgenTemp r) : prepFormals' bs n (r + 1)

newFrame :: Symbol -> [Bool] -> Frame
newFrame nm fs = defaultFrame { name = nm, formals = fs }

externalCall :: String -> [Exp] -> Exp
externalCall s = Call (Name $ pack s)

allocArg :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocArg fr Escapa =
  let actual = actualArg fr
      acc    = InFrame $ actual * wSz + argsGap
  in  return
        (fr { formals = formals fr ++ [True], actualArg = actual + 1 }, acc)
allocArg fr NoEscapa = if actualArg fr < 4
  then
    let
      actual = actualArg fr
      t      = argregs !! actual
    in
      return
        ( fr { formals = formals fr ++ [False], actualArg = actual + 1 }
        , InReg t
        )
  else
    let actual = actualArg fr
        acc    = InFrame $ actual * wSz + argsGap
    in  return
          (fr { formals = formals fr ++ [True], actualArg = actual + 1 }, acc)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ -(actual * wSz + localsGap)
  in  return
        (fr { locals = locals fr ++ [True], actualLocal = actual + 1 }, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr { locals = locals fr ++ [False] }, InReg s)

allocLocalStack :: (Monad w) => Frame -> w (Frame, Access)
allocLocalStack fr =
  let actual = actualLocal fr
      acc    = InFrame $ -(actual * wSz + localsGap)
  in  return
        (fr { locals = locals fr ++ [True], actualLocal = actual + 1 }, acc)

auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n = Mem (Binop Plus (auxexp (n - 1)) (Const fpPrevLev))

exp :: Access -> Int -> Exp
exp (InFrame k) e = Mem (Binop Plus (auxexp e) (Const k))
exp (InReg   l) _ = Temp l

callArgs :: (Monad w) => Int -> Frame -> w Frame
callArgs i fr =
  let old = maxParams fr
      new = max old i
  in  return fr { maxParams = new }

procEntryExit2 :: Frame -> [Instr] -> [Instr]
procEntryExit2 fr bd =
  bd
    ++ [ OPER
           { assem = Empty
           , srcs  = [zero, ra, sp, rv] -- ++ callersaves
           , dsts  = []
           , jump  = Nothing
           }
       ]

procEntryExit3 :: Frame -> [Temp] -> [Instr] -> [Instr]
procEntryExit3 f temps bd =
  let
    saved_temps      = L.intersect calleesaves temps
    n_locals         = actualLocal f
    n_args           = maxParams f
    n_saved          = L.length saved_temps
    saved_ofst       = n_args * wSz
    fp_ofst          = saved_ofst + n_saved * wSz
    ra_ofst          = fp_ofst + wSz
    -- frame format: [locals, padding, ra, fp, saved temps, outgoing args]
    stack_sz_pre     = (n_locals + 2 + n_saved + n_args)
    aligned          = even stack_sz_pre
    padding          = if aligned then 0 else 1
    stack_sz         = (stack_sz_pre + padding) * wSz
    ra_ofst_from_top = -(n_locals + padding + 1) * wSz
    fp_ofst_from_top = ra_ofst_from_top - wSz
    pre_dirs         = [DIR TEXT, DIR (ALIGN 2), DIR (GLOBL (name f))]
    fp_sp_ra_entry =
      [ --DIR (FRAME fp stack_sz ra)
        OPER
        { assem = SW fp fp_ofst_from_top sp
        , dsts  = []
        , srcs  = []
        , jump  = Nothing
        }
      , OPER
        { assem = SW ra ra_ofst_from_top sp
        , dsts  = []
        , srcs  = []
        , jump  = Nothing
        }
      , MOV {assem = MOVE fp sp, dst = fp, src = sp}
      , OPER
        { assem = ADDI sp sp (-stack_sz)
        , dsts  = []
        , srcs  = []
        , jump  = Nothing
        }
      ]
    fp_sp_ra_exit
      = [ MOV {assem = MOVE sp fp, dst = sp, src = fp}
        , OPER
          { assem = LW fp sp fp_ofst_from_top
          , dsts  = []
          , srcs  = []
          , jump  = Nothing
          }
        , OPER
          { assem = LW ra sp ra_ofst_from_top
          , dsts  = []
          , srcs  = []
          , jump  = Nothing
          }
        ]
    saved_stores = L.map
      (\(s, i) -> OPER
        { assem = SW s (saved_ofst + i * wSz) sp
        , dsts  = []
        , srcs  = []
        , jump  = Nothing
        }
      )
      (L.zip saved_temps [0 ..])
    saved_restores = L.map
      (\(s, i) -> OPER
        { assem = LW s sp (saved_ofst + i * wSz)
        , dsts  = []
        , srcs  = []
        , jump  = Nothing
        }
      )
      (L.zip saved_temps [0 ..])
    label    = [LABEL {assem = Lbl (name f), lab = name f}]
    prologue = pre_dirs ++ label ++ fp_sp_ra_entry ++ saved_stores
    epilogue =
      saved_restores
        ++ fp_sp_ra_exit
        ++ [ OPER {assem = J ra, dsts = [], srcs = [rv], jump = Just []}
           , OPER {assem = NOP, dsts = [], srcs = [], jump = Nothing}
           ]
  in
    prologue ++ bd ++ epilogue
