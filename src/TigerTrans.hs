{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TigerTrans where

import qualified Control.Monad.State           as ST
import           Prelude                 hiding ( seq
                                                , Ordering(..)
                                                )
import qualified Prelude                       as P
                                                ( Ordering(..)
                                                , error
                                                )
import qualified TigerAbs                      as Abs
import           TigerErrores
import           TigerFrame                    as F
import           TigerSymbol                   as T
import           TigerTemp
import           TigerTree

import           Control.Monad
import qualified Data.Foldable                 as Fold
import           Data.List                     as List
import           Data.Ord                hiding ( EQ
                                                , GT
                                                , LT
                                                )
import           TigerExterna

import           Debug.Trace

type TransFrag = Frag -- Reexport Fragtype

data IsProc = IsProc | IsFun

-- | Empaquetadores de expresiones
data BExp = Ex Exp | Nx Stm | Cx ((Label, Label) -> Stm)

instance Show BExp where
    show (Ex e)  = "Ex " ++ show e
    show (Nx e)  = "Nx " ++ show e
    show (Cx _ ) = "Cx "

-- | Los niveles son un stack de (Frame, Int)
-- Recordar que Frame es una representación del Marco Virtual.
data LevelI = MkLI {getFrame' :: Frame, getNlvl' :: Int}
  deriving Show

type Level = [LevelI]

-- | Helpers de niveles.
getFrame :: Level -> Frame
getFrame = getFrame' . head

getNlvl :: Level -> Int
getNlvl = getNlvl' . head

setFrame :: Frame -> Level -> Level
setFrame f (MkLI _ l : xs) = MkLI f l : xs
setFrame _ _               = P.error "setFrame"

newLevel :: Level -> Symbol -> [Bool] -> Level
newLevel []                  s bs = [MkLI (newFrame s bs) 0]
newLevel ls@(MkLI _ lvl : _) s bs = MkLI (newFrame s bs) (lvl + 1) : ls

getParent :: Level -> Level
getParent []       = P.error "No fuimos del outermost level"
getParent (_ : xs) = xs

outermost :: Level
outermost = [MkLI (newFrame (pack "_undermain") []) (-1)]

-- | Clase encargada del manejo de memoria y niveles.
-- Esta etapa va a consumir el AST y construir un nuevo
-- lenguaje llamado Código Intermedio.
-- En este proceso vamos tomando nota cuantas variables define
-- una función o let, para eventualmente crear los marcos necesarios
-- para le ejecución de código assembler.
class (Monad w, TLGenerator w, Demon w) => MemM w where
    -- | Level management
    getActualLevel :: w Int
    -- | Salida management
    pushSalida :: Maybe Label -> w ()
    topSalida :: w (Maybe Label)
    popSalida :: w ()
    -- | Level management Cont.
    pushLevel :: Level -> w ()
    popLevel  :: w ()
    topLevel  :: w Level
    -- | Pedimos memoria para una variable local
    allocLocal :: Abs.Escapa -> w Access
    allocLocal e = do
        t <- topLevel
        popLevel
        (f,acc) <- F.allocLocal (getFrame t) e
        let nt = setFrame f t
        pushLevel nt
        return  acc
    -- | Pedimos memoria para un argumento
    allocArg :: Abs.Escapa -> w Access
    allocArg e = do
        t <- topLevel
        popLevel
        (f,a) <- F.allocArg (getFrame t) e
        pushLevel (setFrame f t)
        return a
    -- | Registro de numero de argumentos salientes
    callArgs :: Int -> w ()
    callArgs n = do
      t <- topLevel
      popLevel
      nf <- F.callArgs (n + 1) (getFrame t)
      pushLevel (setFrame nf t)
    -- | Frag management
    -- Básicamente los fragmentos van a ser un efecto
    -- lateral de la computación.
    pushFrag  :: Frag -> w ()
    getFrags  :: w [Frag]

-- | Generación de código intermedio.
-- Cada construcción del [AST](src/TigerAbs.hs) la consumiremos
-- y construiremos un fragmento de código intermedio que eventualmente
--  se traducirá en código de máquina y ejecutará.
-- Algunas funciones se especializan más para conseguir un mejor código intermedio.
class IrGen w where
    procEntryExit :: Level -> BExp -> w ()
    unitExp :: w BExp
    nilExp :: w BExp
    intExp :: Int -> w BExp
    stringExp :: Symbol -> w BExp
    simpleVar :: Access -> Int -> w BExp
    varDec :: Access -> w BExp
    fieldVar :: BExp -> Int -> w BExp
    subscriptVar :: BExp -> BExp -> w BExp
    recordExp :: [(BExp,Int)]  -> w BExp
    callExp :: Label -> Externa -> IsProc -> Level -> [BExp] -> w BExp
    letExp :: [BExp] -> BExp -> w BExp
    breakExp :: w BExp
    seqExp :: [BExp] -> w BExp
    preWhileforExp :: w ()
    posWhileforExp :: w ()
    whileExp :: BExp -> BExp -> w BExp
    forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    ifThenExp :: BExp -> BExp -> w BExp
    ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    assignExp :: BExp -> BExp -> w BExp
    preFunctionDec :: Level -> w ()
    posFunctionDec :: w ()
    functionDec :: BExp -> Level -> Externa -> w BExp
    binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    --binOpIntRelExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    arrayExp :: BExp -> BExp -> w BExp

-- | Función helper seq
seq :: [Stm] -> Stm
seq []       = ExpS $ Const 0
seq [s     ] = s
seq (x : xs) = Seq x (seq xs)

-- | Funcion helper
recorre :: Int -> Temp -> [Stm]
recorre 0 _ = []
recorre n t =
  Move (Temp t) (Mem (Binop Plus (Temp t) (Const fpPrevLev)))
    : recorre (n - 1) t


-- | Des-empaquetador de expresiones
-- Es momadico ya que deberá crear labels, y temps
-- para des-empaquetar una condición.
unEx :: (Monad w, TLGenerator w) => BExp -> w Exp
unEx (Ex e ) = return e
unEx (Nx s ) = return $ Eseq s (Const 0)
unEx (Cx cf) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  return $ Eseq
    (seq
      [ Move (Temp r) (Const 1)
      , cf (t, f)
      , Label f
      , Move (Temp r) (Const 0)
      , Label t
      ]
    )
    (Temp r)


-- | Des-empaquetador de statements (cosas que no retornan valor)
unNx :: (Monad w, TLGenerator w) => BExp -> w Stm
unNx (Ex e ) = return $ ExpS e
unNx (Nx s ) = return s
unNx (Cx cf) = do
  t <- newLabel
  return $ seq [cf (t, t), Label t]

-- | Des-empaquetador de condiciones
unCx :: (Monad w, TLGenerator w, Demon w) => BExp -> w ((Label, Label) -> Stm)
unCx (Nx s        ) = internal $ pack "unCx(Nx...)"
unCx (Cx cf       ) = return cf
-- Pequeña optimización boluda
unCx (Ex (Const 0)) = return (\(_, f) -> Jump (Name f) f)
unCx (Ex (Const _)) = return (\(t, _) -> Jump (Name t) t)
unCx (Ex e        ) = return (uncurry (CJump NE e (Const 0)))

instance (MemM w) => IrGen w where
    procEntryExit lvl bd =  do
        bd' <- unNx bd
        let res = Proc bd' (getFrame lvl)
        pushFrag res
    stringExp t = do
        l <- newLabel
        let ln = T.append (pack ".word ")  (pack $ show $ T.length t)
        let str = T.append (T.append (pack ".ascii \"") t) (pack "\\000\"")
        pushFrag $ AString l [ln,str]
        return $ Ex $ Name l
    preFunctionDec lvl = do
        pushSalida Nothing  -- In case a break is called.
        pushLevel lvl
    posFunctionDec = popSalida >> popLevel
    -- functionDec :: BExp -> Level -> Externa -> w BExp
    functionDec bd lvl ext = do
        body <- case ext of
                  Runtime -> unNx bd
                  Propia  -> Move (Temp rv) <$> unEx bd
        l <- topLevel
        procEntryExit l (Nx body)
        return $ Ex $ Const 0
    -- simpleVar :: Access -> Int -> w BExp
    simpleVar (InReg t) lvl = return $ Ex (Temp t)
    simpleVar (InFrame ofst) lvl = do
      actuallvl <- getActualLevel
      tmpRecorre <- newTemp
      case compare actuallvl lvl of
        P.LT -> derror $ pack "Error: Variable out of scope"
        P.EQ -> return $ Ex (Mem (Binop Plus (Temp fp) (Const ofst)))
        P.GT -> return $ Ex (Eseq
                  (seq (Move (Temp tmpRecorre) (Temp fp) : recorre (actuallvl - lvl) tmpRecorre))
                  (Mem (Binop Plus (Temp tmpRecorre) (Const ofst))))
    varDec acc = do { i <- getActualLevel; simpleVar acc i}
    unitExp = return $ Ex (Const 0)
    nilExp = return $ Ex (Const 0)
    intExp i = return $ Ex (Const i)
    fieldVar var pos = do
      car <- unEx var
      return $ Ex $ Eseq (seq
        [ExpS (F.externalCall "_checkNil" [car])])
        (Mem (Binop Plus car (Const (pos * wSz))))
    -- subscriptVar :: BExp -> BExp -> w BExp
    subscriptVar var ind = do
        evar <- unEx var
        eind <- unEx ind
        tvar <- newTemp
        tind <- newTemp
        return $ Ex $
            Eseq
                (seq    [Move (Temp tvar) evar
                        ,Move (Temp tind) eind
                        ,ExpS $ externalCall "_checkIndexArray" [Temp tvar, Temp tind]])
                (Mem $ Binop Plus (Temp tvar) (Binop Mul (Temp tind) (Const wSz)))
    -- recordExp :: [(BExp,Int)]  -> w BExp
    recordExp flds = do
      -- flds' :: [Temp, Stm, Int] (En donde va, Move, indice)
      flds' <- mapM (\(be, i) -> do
                       tmp <- newTemp
                       ex <- unEx be
                       return (Temp tmp, Move (Temp tmp) ex, i)) flds
      let orderedTmps = (\(x,_,_)->x) <$> List.sortBy (\(_,_,a)(_,_,b) -> compare a b) flds'
      return $ Ex $ Eseq
                      (seq ((\(_,x,_)->x) <$> flds'))
                      (externalCall "_allocRecord" (Const (List.length orderedTmps) : orderedTmps))
    -- callExp :: Label -> Externa -> IsProc -> Level -> [BExp] -> w BExp
    callExp name Propia isproc calllvl args = do
      cargs <- mapM (\e -> do
                      t <- newTemp
                      e' <- unEx e
                      return (Temp t, Move (Temp t) e')) args
      tmp <- newTemp
      let cargs' = fst <$> cargs
      let ins = snd <$> cargs
      tmpsl_ <- newTemp
      actuallvl <- getActualLevel
      let tmpsl = Temp tmpsl_
      tmpRecorre <- newTemp
      let calllvlN = getNlvl calllvl
      let inssl = Move tmpsl (case compare actuallvl calllvlN of
                                P.LT -> Temp fp
                                P.EQ -> Mem (Binop Plus (Temp fp) (Const fpPrevLev))
                                P.GT -> Eseq
                                          (seq (Move (Temp tmpRecorre) (Temp fp) : recorre (actuallvl - calllvlN) tmpRecorre))
                                          (Temp tmpRecorre)
                             )
      case isproc of
        IsProc -> return $ Nx $ seq (ins ++ [inssl] ++ [ExpS (Call (Name name) (tmpsl : cargs'))])
        IsFun -> return $ Ex $ Eseq (seq ((ins ++ [inssl]) ++
                    [ExpS (Call (Name name) (tmpsl : cargs'))
                    , Move (Temp tmp) (Temp rv)
                    ]))
                    (Temp tmp)
    -- callExp :: Label -> Externa -> Bool -> Level -> [BExp] -> w BExp
    callExp name Runtime isproc calllvl args = do
      cargs <- mapM (\e -> do
                      t <- newTemp
                      e' <- unEx e
                      return (Temp t, Move (Temp t) e')) args
      tmp <- newTemp
      let cargs' = fst <$> cargs
      let ins = snd <$> cargs
      case isproc of
        IsProc -> return $ Nx $ seq (ins ++ [ExpS (externalCall (unpack name) cargs')])
        IsFun ->  return $ Ex $ Eseq (seq (ins ++
                    [ExpS (externalCall (unpack name) cargs')
                    , Move (Temp tmp) (Temp rv)
                    ]))
                    (Temp tmp)
    -- letExp :: [BExp] -> BExp -> w BExp
    letExp [] e = do -- Puede parecer al dope, pero no...
            e' <- unEx e
            return $ Ex e'
    letExp bs body = do
        bes <- mapM unNx bs
        be <- unEx body
        return $ Ex $ Eseq (seq bes) be
    -- breakExp :: w BExp
    breakExp = do
      exitM <- topSalida
      case exitM of
        Just exit ->
          return $ Nx (Jump (Name exit) exit)
        Nothing   -> derror $ pack "Error: break fuera de while"
    -- seqExp :: [BExp] -> w BExp
    seqExp [] = return $ Nx $ ExpS $ Const 0
    seqExp bes = do
        let ret = last bes
        case ret of
            Nx _ -> Nx . seq <$> mapM unNx bes
            ex   -> do
                    let bfront = init bes
                    ess <- mapM unNx bfront
                    ex' <- unEx ex
                    return $ Ex $ Eseq (seq ess) ex'
            --_ -> internal $ pack "WAT!123"
    -- preWhileforExp :: w ()
    preWhileforExp = newLabel >>= pushSalida . Just
    -- posWhileforExp :: w ()
    posWhileforExp = popSalida
    -- whileExp :: BExp -> BExp -> Level -> w BExp
    whileExp cond body = do
        test <- unCx cond
        cody <- unNx body
        init <- newLabel
        bd <- newLabel
        lastM <- topSalida
        case lastM of
            Just last ->
                return $ Nx $ seq
                    [Label init
                    , test (bd,last)
                    , Label bd
                    , cody
                    , Jump (Name init) init
                    , Label last]
            _ -> internal $ pack "no label in salida"
    -- forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    forExp lo hi var body = do
      car <- unEx var
      clo <- unEx lo
      chi <- unEx hi
      cody <- unNx body
      lsigue <- newLabel
      lfin <- newLabel
      lcond <- newLabel
      return $ Nx $ seq
        [Move car clo
        , Label lcond
        , CJump GT car chi lfin lsigue
        , Label lsigue
        , cody
        , Move car (Binop Plus car (Const 1))
        , Jump (Name lcond) lcond
        , Label lfin]
    -- ifThenExp :: BExp -> BExp -> w BExp
    ifThenExp cond bod = do
        test <- unCx cond
        cody <- unNx bod
        lv   <- newLabel
        lf   <- newLabel
        return $ Nx (seq
                [ test (lv,lf)
                , Label lv
                , cody
                , Label lf])
    -- ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExp cond bod els = do
        test <- unCx cond
        cody <- unEx bod
        els' <- unEx els
        lv   <- newLabel
        lf   <- newLabel
        ls   <- newLabel
        tmp  <- newTemp
        return $ Ex $ Eseq (seq
                [ test (lv,lf)
                , Label lv
                , Move (Temp tmp) cody
                , Jump (Name ls) ls
                , Label lf
                , Move (Temp tmp) els'
                , Label ls
                ]) (Temp tmp)
    -- ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit cond bod els = do
        test <- unCx cond
        cody <- unNx bod
        cels <- unNx els
        lv   <- newLabel
        lf   <- newLabel
        ls   <- newLabel
        return $ Nx $ seq
                [ test (lv,lf)
                , Label lv
                , cody
                , Jump (Name ls) ls
                , Label lf
                , cels
                , Label ls
                ]
    -- assignExp :: BExp -> BExp -> w BExp
    assignExp cvar cinit = do
        cvara <- unEx cvar
        cin <- unEx cinit
        case cvara of
            Mem v' ->  do
                t <- newTemp
                return $ Nx $ seq [Move (Temp t) cin, Move cvara (Temp t)]
            _ -> return $ Nx $ Move cvara cin
    -- binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpIntExp le op re = do
      cle <- unEx le
      cre <- unEx re
      case op of
        Abs.PlusOp    -> return $ Ex (Binop Plus cle cre)
        Abs.MinusOp   -> return $ Ex (Binop Minus cle cre)
        Abs.TimesOp   -> return $ Ex (Binop Mul cle cre)
        Abs.DivideOp  -> return $ Ex (Binop Div cle cre)
        Abs.EqOp      -> return $ Cx (uncurry $ CJump EQ cle cre)
        Abs.NeqOp     -> return $ Cx (uncurry $ CJump NE cle cre)
        Abs.LtOp      -> return $ Cx (uncurry $ CJump LT cle cre)
        Abs.LeOp      -> return $ Cx (uncurry $ CJump LE cle cre)
        Abs.GtOp      -> return $ Cx (uncurry $ CJump GT cle cre)
        Abs.GeOp      -> return $ Cx (uncurry $ CJump GE cle cre)
    -- binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpStrExp strl op strr = do
      cstrl <- unEx strl
      cstrr <- unEx strr
      let relop = case op of
                    Abs.EqOp      -> EQ
                    Abs.NeqOp     -> NE
                    Abs.LtOp      -> LT
                    Abs.LeOp      -> LE
                    Abs.GtOp      -> GT
                    Abs.GeOp      -> GE
      return $ Cx (uncurry $ CJump relop
                                        (externalCall "_stringCompare" [cstrl, cstrr])
                                        (Const 0))
    --binOpIntRelExp le op re = do -- TODO: Por ahi se puede dividir y optimizar
    -- arrayExp :: BExp -> BExp -> w BExp
    arrayExp size init = do
        sz <- unEx size
        ini <- unEx init
        t <- newTemp
        return $ Ex $ Eseq (seq
                [ExpS $ externalCall "_initArray" [sz,ini]
                , Move (Temp t) (Temp rv)
                ]) (Temp t)
