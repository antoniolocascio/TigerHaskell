{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module TigerSeman where

import           TigerAbs
import           TigerErrores                  as E
import           TigerSres
import           TigerTips
import           TigerUnique

-- Segunda parte imports:
import           TigerTemp
import           TigerTrans
import qualified TigerTree                     as Tree
import qualified TigerFrame                    as F

-- Monads
import           Control.Monad.State
import           Control.Monad.Trans.Except

import           TigerSymbol

import qualified Control.Conditional           as C
import           Control.Monad
-- Data
import           Data.List                     as List
import           Data.Map                      as M
import           Data.Ord                      as Ord

-- Le doy nombre al Preludio.
import           Prelude                       as P

import           Data.Maybe                     ( maybeToList )
import           Debug.Trace                    ( trace )
import           TigerExterna
import           TigerFrame                     ( Frag(..) )
import           TigerTopsort


-- * Análisis Semántico, aka Inferidor de Tipos

-- ** Notas :

-- [outermost] No deberían fallar las búsquedas de variables. Recuerden que
-- el calculo de variables escapadas debería detectar las variables
-- no definidas.

-- [2] En la siguiente etapa vamos a ir generando el código intermedio
-- mezclado con esta etapa por lo que es muy posible que tengan que revisar
-- este modulo. Mi consejo es que sean /lo más ordenados posible/ teniendo en cuenta
-- que van a tener que reescribir bastante.

class (Demon w, Monad w) => Manticore w where
  -- | Inserta una Variable al entorno
    insertValV :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una Función al entorno
    insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- | Inserta una Variable de sólo lectura al entorno
    insertVRO :: Symbol -> ValEntry-> w a -> w a
  -- | Inserta una variable de tipo al entorno
    insertTipoT :: Symbol -> Tipo -> w a -> w a
  -- | Busca una función en el entorno
    getTipoFunV :: Symbol -> w FunEntry
  -- | Busca una variable en el entorno. Ver [1]
    getTipoValV :: Symbol -> w ValEntry
  -- | Busca un tipo en el entorno
    getTipoT :: Symbol -> w Tipo
  -- | Funciones de Debugging!
    showVEnv :: w a -> w a
    showTEnv :: w a -> w a
    --
    -- | Función monadica que determina si dos tipos son iguales.
    -- El catch está en que tenemos una especie de referencia entre los
    -- nombres de los tipos, ya que cuando estamos analizando la existencia de bucles
    -- en la definición permitimos cierto alias hasta que los linearizamos con el
    -- sort topológico.
    tiposIguales :: Tipo -> Tipo -> w Bool
    tiposIguales (RefRecord s) l@(TRecord _ u) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales ls l
            _ -> E.internal $ pack "No son tipos iguales... 123+1"
    tiposIguales l@(TRecord _ u) (RefRecord s) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales l ls
            _ -> E.internal $ pack "No son tipos iguales... 123+2"
    tiposIguales (RefRecord s) (RefRecord s') = do
        s1 <- getTipoT s
        s2 <- getTipoT s'
        tiposIguales s1 s2
    tiposIguales TNil  (RefRecord _) = return True
    tiposIguales (RefRecord _) TNil = return True
    tiposIguales (RefRecord _) _ = E.internal $ pack "No son tipos iguales... 123+3"
    tiposIguales  e (RefRecord s) = E.internal $ pack $ "No son tipos iguales... 123+4" ++ (show e ++ show s)
    tiposIguales a b = return (equivTipo a b)
    --
    -- | Generador de uniques. Etapa 2
    --
    ugen :: w Unique

-- | Definimos algunos helpers

-- | `addpos` nos permite agregar información al error.
addpos :: (Demon w, Show b) => w a -> b -> w a
addpos t p = E.adder t (pack $ show p)

-- | Patrón de errores...
errorTiposMsg :: (Demon w, Show p) => p -> String -> Tipo -> Tipo -> w a
errorTiposMsg p msg t1 t2 =
  flip addpos p $ flip adder (pack msg) $ errorTipos t1 t2

depend :: Ty -> [Symbol]
depend (NameTy   s ) = [s]
depend (ArrayTy  s ) = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts


-- | Función auxiliar que chequea cuales son los tipos
-- comparables.
-- Por ejemplo, ` if nil = nil then ...` es una expresión ilegal
-- ya que no se puede determinar el tipo de cada uno de los nils.
-- Referencia: [A.3.Expressions.Nil]
tiposComparables :: Tipo -> Tipo -> Oper -> Bool
tiposComparables TNil  TNil EqOp  = False
tiposComparables TUnit _    EqOp  = False
tiposComparables _     _    EqOp  = True
tiposComparables TNil  TNil NeqOp = False
tiposComparables TUnit _    NeqOp = False
tiposComparables _     _    NeqOp = True
tiposComparables _     _    _     = True

-- | Función que chequea que los tipos de los campos sean los mismos
-- Ver 'transExp (RecordExp ...)'
-- Ver 'transExp (CallExp ...)'

andM :: (Monad m) => m Bool -> m Bool -> m Bool
andM m1 m2 = do
  e1 <- m1
  e2 <- m2
  return $ e1 && e2

cmpZip
  :: (Demon m, Monad m, Manticore m)
  => [(Symbol, Tipo)]
  -> [(Symbol, Tipo, Int)]
  -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _ = derror $ pack "Diferencia en la cantidad. 1"
cmpZip _ [] = derror $ pack "Diferencia en la cantidad. 2"
cmpZip ((sl, tl) : xs) ((sr, tr, p) : ys) = C.ifM
  (tiposIguales tl tr `andM` return (sl == sr))
  (cmpZip xs ys)
  (errorTipos tl tr)

cmpZip2 :: (Demon m, Monad m, Manticore m) => [Tipo] -> [Tipo] -> m () --Bool
cmpZip2 [] [] = return ()
cmpZip2 [] _  = derror $ pack "Diferencia en la cantidad. 1"
cmpZip2 _  [] = derror $ pack "Diferencia en la cantidad. 2"
cmpZip2 (tl : xs) (tr : ys) =
  C.ifM (tiposIguales tl tr) (cmpZip2 xs ys) (errorTipos tl tr)

allDifferent :: [Symbol] -> Bool
allDifferent xs = P.length xs == P.length (group xs)

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe (Tipo, Int)
buscarM s [] = Nothing
buscarM s ((s', t, i) : xs) | s == s'   = Just (t, i)
                            | otherwise = buscarM s xs

mostRestrictive :: Tipo -> Tipo -> Tipo
mostRestrictive TNil            t@(TRecord _ _) = t
mostRestrictive t@(TRecord _ _) TNil            = t
mostRestrictive (  TInt RO    ) (TInt _ )       = TInt RO
mostRestrictive (  TInt _     ) (TInt RO)       = TInt RO
mostRestrictive t               _               = t


-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está **accediendo**.
transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
--transVar :: (Manticore w) => Var -> w ( () , Tipo)
transVar (SimpleVar s) = do
  (t, acc, lvl) <- getTipoValV s
  ce            <- simpleVar acc lvl
  return (ce, t)

transVar (FieldVar v s) = do
  (ce, tv) <- transVar v
  case tv of
    (TRecord fs _) -> case buscarM s fs of
      Just (ty, i) -> do
        ce' <- fieldVar ce i
        return (ce', ty)
      Nothing -> derror $ pack "Error. Campo no encontrado en record."
    _ -> derror $ pack "Error. No es un record." --ARREGLAR!

transVar (SubscriptVar v e) = do
  (cv, tv) <- transVar v
  (ce, te) <- transExp e
  case tv of
    (TArray ta _) -> if equivTipo te (TInt RW)
      then do
        cv' <- subscriptVar cv ce
        return (cv', ta)
      else derror $ pack "Error. No es int."
    _ -> derror $ pack "Error. No es un array." --ARREGLAR!

-- | __Completar__ 'TransTy'
-- El objetivo de esta función es dado un tipo
-- que proviene de la gramatica, dar una representación
-- de tipo interna del compilador

-- | Nota para cuando se genere código intermedio
-- que 'TransTy ' no necesita ni 'MemM ' ni devuelve 'BExp'
-- porque no se genera código intermedio en la definición de un tipo.
transTy :: (Manticore w) => Ty -> [Symbol] -> w Tipo
transTy (NameTy   s   ) recs = getTipoT s
transTy (RecordTy flds) recs = do
  unless
    (allDifferent (fst <$> flds))
    (derror $ pack "Error. Los campos del Record deben tener nombres distintos."
    )
  let ordered = List.sortBy (Ord.comparing fst) flds
  flds' <- mapM
    (\((s, t), i) -> do
      t' <- case t of
        (NameTy n) ->
          if n `elem` recs then return (RefRecord n) else getTipoT n
        _ -> transTy t recs
      return (s, t', i)
    )
    (zip ordered [0 ..])
  u <- ugen
  return (TRecord flds' u)
transTy (ArrayTy s) recs = do
  t <- getTipoT s
  u <- ugen
  return (TArray t u)


fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _ = P.error "no debería haber una definición de tipos en los args..."

-- | Tip: Capaz que se debería restringir el tipo de 'transDecs'.
-- Tip2: Van a tener que pensar bien que hacen. Ver transExp (LetExp...)
transDecs :: (MemM w, Manticore w) => [Dec] -> [BExp] -> w a -> w ([BExp], a)
transDecs [] bexps m = do
  a <- m
  return (bexps, a)
transDecs (d : ds) bexps m = do
  (codM, (bs, a)) <- transDec d (transDecs ds bexps m)
  let cod = maybeToList codM
  return (cod ++ bs, a)

transDec :: (MemM w, Manticore w) => Dec -> w a -> w (Maybe BExp, a)
transDec (FunctionDec fs) m = do
  unless
    (allDifferent (P.map (\(s, _, _, _, _) -> s) fs))
    (derror $ pack "Error: Funciones con el mismo nombre en un mismo batch")
  lvl <- topLevel
  transFSigs lvl fs $ do
    mapM_
      (\(nm, args, _, body, p) -> do
        unless
          (allDifferent (P.map (\(s, _, _) -> s) args))
          (addpos
            ( derror
            $ pack
                "Error: Parametros con el mismo nombre en declaracion de funcion"
            )
            p
          )
        (lvl, lab, argsTyps, retTy, ext) <- getTipoFunV nm
        preFunctionDec lvl
        (cody, bodyTy) <- transParams
          (zipWith (\(x, e, _) y -> (x, e, y)) args argsTyps)
          lvl
          (transExp body)
        functionDec cody lvl ext
        posFunctionDec
        C.unlessM (tiposIguales bodyTy retTy) $ errorTiposMsg
          p
          "Tipos incosistentes en el cuerpo de una funcion"
          bodyTy
          retTy
      )
      fs
    a <- m
    return (Nothing, a)
 where
  transFSigs
    :: (Manticore w, MemM w)
    => Level
    -> [(Symbol, [(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)]
    -> w a
    -> w a
  transFSigs lvl = P.foldr ((.) . transFSig lvl) id
  transFSig
    :: (Manticore w, MemM w)
    => Level
    -> (Symbol, [(Symbol, Escapa, Ty)], Maybe Symbol, Exp, Pos)
    -> w a
    -> w a
  transFSig actLvl (nm, args, retTy, body, p) env = do
    retTy' <- maybe (return TUnit) getTipoT retTy
    let lbl = appends [nm, pack "_", pack $ posToLabel p]
    argsTyps <- mapM (fromTy . (\(_, _, t) -> t)) args
    let frmls = (\(_, e, _) -> e == Escapa) <$> args
    --let frmls = (\(_, e, _) -> True) <$> args
    let lvl   = newLevel actLvl lbl (True : frmls)
    insertFunV nm (lvl, lbl, argsTyps, retTy', Propia) env

  transParams
    :: (MemM w, Manticore w) => [(Symbol, Escapa, Tipo)] -> Level -> w a -> w a
  transParams []               l m = m
  transParams ((s, e, t) : xs) l m = do
    acc <- allocArg e
    transParams xs l (insertValV s (t, acc, getNlvl l) m)
transDec (VarDec nm escap t init p) m = do
  (cinit, ty) <- transExp init
  when
    (t == Nothing && ty == TNil)
    (addpos
      (derror $ pack
        "Error: Inicializacion de una variable en nil sin declarar su tipo"
      )
      p
    )
  userTy <- maybe (return ty) getTipoT t
  C.unlessM (tiposIguales userTy ty) $ errorTiposMsg
    p
    "Tipos incosistentes en la declaracion de una variable"
    ty
    userTy
  lvlN  <- getActualLevel
  acc   <- allocLocal escap
  cvar  <- simpleVar acc lvlN
  casgn <- assignExp cvar cinit
  insertValV nm (userTy, acc, lvlN) (m >>= \a -> return (Just casgn, a))
transDec (TypeDec xs) m = do
  unless (allDifferent (P.map (\(s, _, _) -> s) xs))
         (derror $ pack "Error: Tipos con el mismo nombre en un mismo batch")
  let deps = List.concatMap dep xs
  let mp   = M.fromList $ (\(x, y, z) -> (x, (y, z))) <$> xs
  ordered <- maybe
    (derror $ pack "Error: Hay un ciclo en la declaracion de tipos")
    return
    (topsort deps)
  let toAdd = ordered ++ (recNames List.\\ ordered)
  addTs toAdd mp $ do
    refs <- getRefs recNames
    let definedNames = recNames List.\\ (fst <$> refs)
    defined <- getDefined definedNames
    let toCorrect = removeRefs refs defined List.\\ M.toList defined
    unless (P.length toCorrect == P.length refs)
      $ internal (pack "Error removiendo referencias")
    correctTs toCorrect (m >>= \a -> return (Nothing, a))
 where
  dep :: (Symbol, Ty, Pos) -> [(Symbol, Symbol)]
  dep (s, NameTy t  , _) = [(t, s)]
  dep (s, ArrayTy t , _) = [(t, s)]
  dep (s, RecordTy l, _) = [ (t, s) | t <- removeRecDeps l ]

  removeRecDeps :: [(Symbol, Ty)] -> [Symbol]
  removeRecDeps [] = []
  removeRecDeps ((_, NameTy t) : xs) | t `elem` recNames = removeRecDeps xs
                                     | otherwise         = t : removeRecDeps xs

  records :: [(Symbol, Ty, Pos)]
  records = P.filter (\(_, t, _) -> isRecord t) xs

  recNames :: [Symbol]
  recNames = (\(s, _, _) -> s) <$> records

  isRecord :: Ty -> Bool
  isRecord (RecordTy _) = True
  isRecord _            = False

  addTs :: (Manticore w) => [Symbol] -> M.Map Symbol (Ty, Pos) -> w a -> w a
  addTs nms mp = P.foldr ((.) . addT mp) id nms

  addT :: (Manticore w) => M.Map Symbol (Ty, Pos) -> Symbol -> w a -> w a
  addT mp s m = do
    let (ty, p) = mp M.! s
    t <- transTy ty recNames
    insertTipoT s t m

  correctTs :: (Manticore w) => [(Symbol, Tipo)] -> w a -> w a
  correctTs []            m = m
  correctTs ((s, t) : xs) m = correctTs xs $ insertTipoT s t m

  getDefined :: (Manticore w) => [Symbol] -> w (M.Map Symbol Tipo)
  getDefined nms = do
    l <- mapM
      (\n -> do
        t <- getTipoT n
        return (n, t)
      )
      nms
    return $ M.fromList l

  getRefs :: (Manticore w) => [Symbol] -> w [(Symbol, Tipo)]
  getRefs recs = do
    ll <- mapM
      (\s -> do
        t <- getTipoT s
        if hasRef t then return [(s, t)] else return []
      )
      recs
    return $ P.concat ll

  hasRef :: Tipo -> Bool
  hasRef (TRecord flds _) = P.any (\(_, t, _) -> isRef t) flds

  isRef :: Tipo -> Bool
  isRef (RefRecord _) = True
  isRef _             = False

  removeRefs :: [(Symbol, Tipo)] -> M.Map Symbol Tipo -> [(Symbol, Tipo)]
  removeRefs [] yaDef = M.toList yaDef
  removeRefs l@((s, t) : xs) yaDef =
    let yaDef' = removeRef (s, t) yaDef (M.fromList l) in removeRefs xs yaDef'

  removeRef
    :: (Symbol, Tipo)
    -> M.Map Symbol Tipo
    -> M.Map Symbol Tipo
    -> M.Map Symbol Tipo
  removeRef (s, TRecord flds u) yaDef toDef =
    let (flds', yaDef') = tie flds s (M.insert s t yaDef) toDef
        t               = TRecord flds' u
    in  yaDef'


  tie
    :: [(Symbol, Tipo, Posicion)]
    -> Symbol
    -> M.Map Symbol Tipo
    -> M.Map Symbol Tipo
    -> ([(Symbol, Tipo, Posicion)], M.Map Symbol Tipo)
  tie [] _ def _ = ([], def)
  tie ((n, RefRecord r, p) : flds) s def toDef
    | r == s
    = let (rest, retM) = tie flds s def toDef
      in  ((n, def M.! s, p) : rest, retM)
    | otherwise
    = case M.lookup r def of
      (Just t2) ->
        let (rest, retM) = tie flds s def toDef in ((n, t2, p) : rest, retM)
      Nothing -> case M.lookup r toDef of
        (Just t2@(TRecord fs u)) ->
          let newM         = M.insert r t' def
              (fs', def')  = tie fs r newM toDef
              t'           = TRecord fs' u
              (rest, retM) = tie flds s def' toDef
          in  ((n, t', p) : rest, retM)
        (Just t) -> ([], M.empty) --Error!
        _        -> ([], M.empty) --Error!
  tie ((n, t, p) : flds) s m td =
    let (rest, retM) = tie flds s m td in ((n, t, p) : rest, retM)


transExp :: (MemM w, Manticore w) => Exp -> w (BExp, Tipo)
--transExp :: (Manticore w) => Exp -> w (() , Tipo)
transExp (VarExp v p)        = addpos (transVar v) p
transExp UnitExp{}           = fmap (, TUnit) unitExp -- ** return ((), TUnit)
transExp NilExp{}            = fmap (, TNil) nilExp -- ** return ((), TNil)
transExp (IntExp    i _    ) = fmap (, TInt RW) (intExp i) -- ** return ((), TInt RW)
transExp (StringExp s _    ) = fmap (, TString) (stringExp (pack s)) -- ** return (() , TString)
transExp (CallExp nm args p) = do
  callArgs (P.length args)
  (lvl, lbl, fTyps, rTyp, ext) <- addpos (getTipoFunV nm) p
  args'                        <- mapM transExp args
  flip addpos p $ cmpZip2 (snd <$> args') fTyps
  let isProc = case rTyp of
        TUnit -> IsProc
        _     -> IsFun
  case ext of
    Runtime -> do
      ccall <- callExp lbl ext isProc lvl (fst <$> args')
      return (ccall, rTyp)
    Propia -> do
      ccall <- callExp lbl ext isProc lvl (fst <$> args')
      return (ccall, rTyp)

transExp (OpExp el' oper er' p) = do -- Esta va /gratis/
  (cel, el) <- transExp el'
  (cer, er) <- transExp er'
  case oper of
    EqOp -> if tiposComparables el er EqOp
      then oOps el er cel cer
      else addpos
        (derror
          (pack
            (  "Error de Tipos. Tipos no comparables: "
            ++ show el
            ++ ", "
            ++ show er
            )
          )
        )
        p
    NeqOp -> if tiposComparables el er NeqOp
      then oOps el er cel cer
      else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
    -- Los unifico en esta etapa porque solo chequeamos los tipos, en la próxima
    -- tendrán que hacer algo más interesante.
    PlusOp   -> oOpsInt el er cel cer
    MinusOp  -> oOpsInt el er cel cer
    TimesOp  -> oOpsInt el er cel cer
    DivideOp -> oOpsInt el er cel cer
    LtOp     -> oOpsIntStr el er cel cer
    LeOp     -> oOpsIntStr el er cel cer
    GtOp     -> oOpsIntStr el er cel cer
    GeOp     -> oOpsIntStr el er cel cer
 where
  oOps l r cel cer = if equivTipo l r
    then case l of
      TString -> do
        cop <- binOpStrExp cel oper cer
        return (cop, TInt RO)
      _ -> do
        cop <- binOpIntExp cel oper cer
        return (cop, TInt RO)
    else compE
  oOpsInt l r cel cer = if equivTipo l (TInt RW)
    then
      (if equivTipo l r
        then do
          cop <- binOpIntExp cel oper cer
          return (cop, TInt RO)
        else compE
      )
    else addpos
      (derror
        (pack "Error, una de las expresiones de la operacion no es entera.")
      )
      p
  oOpsIntStr l r cel cer = if equivTipo l r
    then case l of
      (TInt _) -> do
        cop <- binOpIntExp cel oper cer
        return (cop, TInt RO)
      TString -> do
        cop <- binOpStrExp cel oper cer
        return (cop, TInt RO)
      _ -> addpos
        (derror (pack "Error, las expresiones no se pueden comparar por orden.")
        )
        p
    else compE




  compE = addpos (derror (pack "Error en el chequeo de una comparación.")) p
  -- TODO: Mejorar aca, emprolijar. Si separamos las ops tambien cambiar.
-- | Recordemos que 'RecordExp :: [(Symbol, Exp)] -> Symbol -> Pos -> Exp'
-- Donde el primer argumento son los campos del records, y el segundo es
-- el texto plano de un tipo (que ya debería estar definido).
transExp (RecordExp flds rt p) = addpos (getTipoT rt) p >>= \case -- Buscamos en la tabla que tipo es 'rt', y hacemos un análisis por casos.
  trec@(TRecord fldsTy _) -> -- ':: TRecord [(Symbol, Tipo, Int)] Unique'
                             do
      -- Especial atención acá.
      -- Tenemos una lista de expresiones con efectos
      -- y estos efectos tiene producirse en orden! 'mapM' viene a mano.
    fldsTys <- mapM (\(nm, cod) -> (nm, ) <$> transExp cod) flds -- Buscamos los tipos de cada una de los campos.
    -- como resultado tenemos 'fldsTys :: (Symbol, ( CIr , Tipo))'
    -- Lo que resta es chequear que los tipos  sean los mismos, entre los que el programador dio
    -- y los que tienen que ser según la definición del record.
    let ordered = List.sortBy (Ord.comparing fst) fldsTys
    -- asumiendo que no nos interesan como el usuario ingresa los campos los ordenamos.
    flip addpos p $ cmpZip ((\(s, (c, t)) -> (s, t)) <$> ordered) fldsTy -- Demon corta la ejecuci��n.
    let bexpFlds = (\(_, (be, _)) -> be) <$> ordered
    let cflds    = zip bexpFlds [0 ..]
    crec <- recordExp cflds
    return (crec, trec) -- Si todo fue bien devolvemos trec.
  _ -> flip addpos p $ derror (pack "Error de tipos.")
transExp (SeqExp es p) = --last <$> mapM transExp es
-- ^ Notar que esto queda así porque no nos interesan los
-- units intermedios. Eventualmente vamos a coleccionar los códigos intermedios y se verá algo similar a:
                         do
  ces  <- mapM transExp es
  cseq <- seqExp $ fst <$> ces
  return (cseq, snd $ last ces)
transExp (AssignExp var val p) = do
  (cvar, varTy) <- transVar var
  (cval, valTy) <- transExp val
  when
    (varTy == TInt RO)
    (addpos
      (derror $ pack
        ("Error: Asignacion de una variable de solo lectura: " ++ show var)
      )
      p
    )
  C.unlessM (tiposIguales varTy valTy)
    $ errorTiposMsg p "En asignacion:" varTy valTy
  cass <- assignExp cvar cval
  return (cass, TUnit)
transExp (IfExp co th Nothing p) = do
  -- Analizamos el tipo de la condición
  (ccond, co') <- transExp co
-- chequeamos que sea un entero.
  unless (equivTipo co' TBool)
    $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
-- Analizamos el tipo del branch.
  (cth, th') <- transExp th
-- chequeamos que sea de tipo Unit.
  unless (equivTipo th' TUnit)
    $ errorTiposMsg p "En el branch del if->" th' TUnit
  cif <- ifThenExp ccond cth
-- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
  return (cif, TUnit) -- ** return (ifThenExp ccond cth, TUnit)
transExp (IfExp co th (Just el) p) = do
  (ccond, condType) <- transExp co
  unless (equivTipo condType TBool)
    $ errorTiposMsg p "En la condición del if ->" condType TBool
  (cth, ttType) <- transExp th
  (cel, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType)
    $ errorTiposMsg p "En los branches:" ttType ffType
  cif <- ifThenElseExp ccond cth cel
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  return (cif, mostRestrictive ttType ffType) -- ** return (ifThenElseExp ccond cth cel, mostRestrictive ttType ffType)
transExp (WhileExp co body p) = do
  (ccond, coTy) <- transExp co
  unless (equivTipo coTy TBool)
    $ errorTiposMsg p "En la condición del While:" coTy TBool
  preWhileforExp
  (cbody, boTy) <- transExp body
  unless (equivTipo boTy TUnit)
    $ errorTiposMsg p "En el cuerpo del While:" boTy TUnit
  cwh <- whileExp ccond cbody
  posWhileforExp
  return (cwh, TUnit)
transExp (ForExp nv mb lo hi bo p) = do
  (clo, loTy) <- transExp lo
  unless (equivTipo loTy (TInt RW))
    $ errorTiposMsg p "En la expresion low del For:" loTy (TInt RW)
  (chi, hiTy) <- transExp hi
  unless (equivTipo hiTy (TInt RW))
    $ errorTiposMsg p "En la expresion high del For:" hiTy (TInt RW)
  preWhileforExp
  lcl          <- allocLocal mb
  lvlN         <- getActualLevel
  car          <- simpleVar lcl lvlN
  (cody, boTy) <- insertVRO nv (TInt RO, lcl, lvlN) $ transExp bo
  cfr          <- forExp clo chi car cody
  posWhileforExp
  unless (equivTipo boTy TUnit)
    $ errorTiposMsg p "En el cuerpo del For:" boTy TBool
  return (cfr, TUnit)
transExp (LetExp dcs body p) = do
  (cdecs, (cody, boTy)) <- transDecs dcs [] (transExp body)
  clet                  <- letExp cdecs cody
  return (clet, boTy)
transExp (BreakExp p) = do
  cbr <- breakExp
  return (cbr, TUnit)
transExp (ArrayExp sn cant init p) = addpos (getTipoT sn) p >>= \case
  tar@(TArray elemTy _) -> do
    (csz, cantTy) <- transExp cant
    unless (equivTipo cantTy (TInt RW))
      $ errorTiposMsg p "En la cantidad del Array" cantTy (TInt RW)
    (cin, initTy) <- transExp init
    unless (equivTipo elemTy initTy)
      $ errorTiposMsg p "En la inicializacion del Array" elemTy initTy
    car <- arrayExp csz cin
    return (car, tar)
  _ -> flip addpos p $ derror (pack "Error de tipos.")

-- Un ejemplo de estado que alcanzaría para realizar todas la funciones es:
data Entorno = Ent {vEnv :: M.Map Symbol EnvEntry, tEnv :: M.Map Symbol Tipo}
    deriving Show
data Acumulador = Ac {lvl :: Level, salida :: [Label], frags :: [Frag]}
    deriving Show

type Estado = (Entorno, Acumulador)

-- Estado Inicial con los entornos
-- * int y string como tipos básicos. -> tEnv
-- * todas las funciones del *runtime* disponibles. -> vEnv
initConf :: Estado
initConf
  = ( Ent
      { tEnv = M.insert (pack "int")
                        (TInt RW)
                        (M.singleton (pack "string") TString)
      , vEnv = M.fromList
        [ ( pack "print"
          , Func (outermost, pack "print", [TString], TUnit, Runtime)
          )
        , (pack "flush" , Func (outermost, pack "flush", [], TUnit, Runtime))
        , (pack "getstr", Func (outermost, pack "getstr", [], TString, Runtime))
        , ( pack "ord"
          , Func (outermost, pack "ord", [TString], TInt RW, Runtime)
          )
        , ( pack "chr"
          , Func (outermost, pack "chr", [TInt RW], TString, Runtime)
          )
        , ( pack "size"
          , Func (outermost, pack "size", [TString], TInt RW, Runtime)
          )
        , ( pack "substring"
          , Func
            ( outermost
            , pack "substring"
            , [TString, TInt RW, TInt RW]
            , TString
            , Runtime
            )
          )
        , ( pack "concat"
          , Func
            (outermost, pack "concat", [TString, TString], TString, Runtime)
          )
        , (pack "not", Func (outermost, pack "not", [TBool], TBool, Runtime))
        , ( pack "exit"
          , Func (outermost, pack "exit", [TInt RW], TUnit, Runtime)
          )
        , ( pack "print_int"
          , Func (outermost, pack "print_int", [TInt RW], TUnit, Runtime)
          )
        ]
      }
    , Ac {lvl = outermost, salida = [], frags = []}
    )

-- Utilizando alguna especie de run de la monada definida, obtenemos algo así
--type Monada = StateT Estado (ExceptT Symbol StGen)
type Monada = ExceptT Symbol (StateT Estado StGen)

modifyEnt :: (Entorno -> Entorno) -> Monada a -> Monada a
modifyEnt f m = do
  (oldEnt, ac) <- get
  put (f oldEnt, ac)
  a               <- m
  (newEnt, newAc) <- get
  put (oldEnt, newAc)
  return a

modifyAc :: (Acumulador -> Acumulador) -> Monada ()
modifyAc f = do
  (ent, oldAc) <- get
  put (ent, f oldAc)
  return ()

instance Demon Monada where
  -- | Levantamos la operación de 'throwE' de la mónada de excepciones.
  derror = throwE
  adder t s = catchE t (throwE . append s)

instance MemM Monada where
  --getActualLevel :: w Int
  getActualLevel = gets (getNlvl . lvl . snd)
  --pushSalida :: Maybe Label -> w ()
  pushSalida = maybe (return ()) (\l -> modifyAc (\a -> a{salida = l : salida a}))
  --topSalida :: w (Maybe Label)
  topSalida = do
    sal <- gets (salida . snd)
    case sal of
      []     -> return Nothing
      (l:ls) -> return (Just l)
  --popSalida :: w ()
  popSalida =  modifyAc (\a -> a{salida = P.drop 1 (salida a)})
  --pushLevel :: Level -> w ()
  pushLevel lev = modifyAc (\a -> a{lvl = lev})
  --popLevel  :: w ()
  popLevel = modifyAc (\a -> a{lvl = getParent (lvl a)})
  --topLevel  :: w Level
  topLevel = gets (lvl . snd)
  --pushFrag  :: Frag -> w ()
  pushFrag fr = modifyAc (\a -> a{frags = fr : frags a})
  --getFrags  :: w [Frag]
  getFrags = gets (frags . snd)

instance Manticore Monada where
  -- | A modo de ejemplo esta es una opción de ejemplo de 'insertValV :: Symbol -> ValEntry -> w a -> w'
    insertValV sym ventry = modifyEnt (\e -> e{ vEnv = M.insert sym (Var ventry) (vEnv e) })
    insertFunV sym fentry = modifyEnt (\e -> e{ vEnv = M.insert sym (Func fentry) (vEnv e) })
    insertVRO sym ventry  = modifyEnt (\e -> e{ vEnv = M.insert sym (Var ventry) (vEnv e) })
    insertTipoT sym t     = modifyEnt (\e -> e{ tEnv = M.insert sym t (tEnv e)})
    getTipoFunV sym = do
      (ent,_) <- get
      case M.lookup sym (vEnv ent) of
        Just (Func fentry) -> return fentry
        Just (Var _)       -> derror $ appends [pack "Error: la variable (", sym, pack ") no es una funcion"]
        Nothing            -> internal (appends [pack "Variable (", sym , pack ") de funcion no se encuentra en la tabla"])
    getTipoValV sym = do
      (ent,_) <- get
      case M.lookup sym (vEnv ent) of
        Just (Var ventry) -> return ventry
        Just (Func _)     -> derror $ appends [pack "Error: la variable (", sym, pack ") es una funcion"]
        Nothing           -> internal (appends [pack "Variable (", sym, pack ") no se encuentra en la tabla"])
    getTipoT sym = do
      (ent,_) <- get
      case M.lookup sym (tEnv ent) of
        Just t  -> return t
        Nothing -> internal (appends [pack "Nombre de Tipo (", sym, pack ") no se encuentra en la tabla"])
    showVEnv m = do
      (ent,_) <- get
      adder m (pack $ show (vEnv ent))
      m
    showTEnv m = do
      (ent,_) <- get
      adder m (pack $ show (tEnv ent))
      m
    ugen = mkUnique

runAndPutBack :: MonadState s m => m b -> s -> m b
runAndPutBack m est = do
  a <- m
  put est
  return a


transProc :: (Manticore w, MemM w) => Exp -> w [Frag]
transProc ast = do
  let pos = Simple {line = 0, col = 0}
  let procExp = LetExp
        [FunctionDec [(pack "_tigermain", [], Just (pack "int"), ast, pos)]]
        (UnitExp pos)
        pos
  transExp procExp
  frags <- getFrags
  getFrags


runMonada :: Monada a -> StGen (Either Symbol a)
runMonada = flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol (BExp, Tipo))
runSeman = runMonada . transExp

runSemanFrags :: Exp -> StGen (Either Symbol [Frag])
runSemanFrags = runMonada . transProc
