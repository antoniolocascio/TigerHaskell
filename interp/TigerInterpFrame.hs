module TigerInterpFrame where

import           TigerTemp
import           TigerTree
import           TigerAbs                       ( Escapa(..) )
import           TigerSymbol
import           TigerAssemTypes
import           Debug.Trace

import           Prelude                 hiding ( exp )

--


-- | Frame pointer
fp :: Temp
fp = pack "fp"

-- | Stack pointer
sp :: Temp
sp = pack "sp"


-- OJO: Aca hay que poner el mismo "rv" que este en TigerFrame
-- | Return value
rv :: Temp
rv = pack "v0"

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
argsGap = wSz
localsGap = 4

-- | Dan inicio a los contadores de argumentos, variables y registros usados.
-- Ver |defaultFrame|
argsInicial, regInicial, localsInicial :: Int
argsInicial = 0
regInicial = 1
localsInicial = 0
paramsInicial = 0

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

instance Show Frag where
    show (Proc s f) = "Frame:" ++ show f ++ '\n': show s
    show (AString l ts) = show l ++ ":\n" ++ (foldr (\t ts -> ("\n\t" ++ unpack t) ++ ts) "" ts)
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
prepFormals' []       _ _ = []
prepFormals' (_ : bs) n r = InFrame n : prepFormals' bs (n + argsGap) r
prepFormals' (False : bs) n r =
  InReg (detgenTemp r) : prepFormals' bs n (r + 1)

newFrame :: Symbol -> [Bool] -> Frame
newFrame nm fs = defaultFrame { name = nm, formals = fs }

externalCall :: String -> [Exp] -> Exp
externalCall s = Call (Name $ pack s)

allocArg :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocArg fr _ =
  let actual = actualArg fr
      acc    = InFrame $ actual * wSz + argsGap
  in  return
        (fr { formals = formals fr ++ [True], actualArg = actual + 1 }, acc)
allocArg fr NoEscapa = do
  s <- newTemp
  return (fr { formals = formals fr ++ [False] }, InReg s)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ -(actual * wSz + localsGap)
  in  return
        (fr { locals = locals fr ++ [True], actualLocal = actual + 1 }, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr { locals = locals fr ++ [False] }, InReg s)

auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n = Mem (Binop Plus (auxexp (n - 1)) (Const fpPrevLev))

exp :: Access -> Int -> Exp
exp (InFrame k) e = Mem (Binop Plus (auxexp e) (Const k))
exp (InReg   l) _ = Temp l
