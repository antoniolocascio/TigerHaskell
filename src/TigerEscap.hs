module TigerEscap where

-- Ocultamos nombres del preludio, pero por ahí usar error es algo
-- que queremos, entonces lo reimportamos calificado.
import           Prelude              hiding (error, lookup)
import qualified Prelude              as P (error)

import qualified Data.Map.Strict      as M

import           TigerAbs
import           TigerErrores
import           TigerSymbol

import           Control.Arrow        (second)
import           Control.Monad        (when)
import           Control.Monad.Trans.Except
import           Control.Monad.State  (get, put)
import qualified Control.Monad.State  as ST

-- Debugging
import           Debug.Trace
  -- Particularmente de Trace usamos la función [trace]
  -- trace :: String -> a -> a


class (Demon m, Monad m) => Escapator m where
    -- Depth Operators
    depth :: m Depth
    -- | Devuelve la profundidad actual.
    up :: m a -> m a
    -- | Ejecuta una computación dada en un entorno
    -- donde se aumento un nivel la profundidad.

    -- *****DEBUGGING!! ********
    printEnv :: m () --
    -- errores
    raise :: Symbol -> m a
    raise = derror

    -- | Actualiza hardcore el entorno
    -- Necesitamos update (y por ende estados)
    -- porque la información de que una variable escapa
    -- no está en Var.
    update :: Symbol -> Escapa -> m ()
    -- | Busca el symbolo en el entorno
    lookup :: Symbol -> m (Maybe (Int, Escapa))
    -- | Permite agregar variables a un entorno local.
    -- ```insert name esc computacion```
    -- computacion tiene un entorno[name -> esc]
    insert :: Symbol -> Escapa -> m a -> m a

lookUpLvl :: (Escapator m) => Symbol -> m Int
lookUpLvl nm = lookup nm >>= maybe (notfound nm) (return . fst)

travVar :: (Escapator m) => Var -> m Var
travVar (SimpleVar s) = do
    lvl <- lookUpLvl s
    actLvl <- depth
    when (actLvl > lvl) (update s Escapa)
    return (SimpleVar s)
travVar (FieldVar v p) = do
    v' <- travVar v
    return (FieldVar v' p)
travVar (SubscriptVar v e) = do
        v' <- travVar v
        e' <- travExp e
        return (SubscriptVar v' e')

travExp :: (Escapator m) => Exp -> m Exp
travExp (VarExp v p) = do
    v' <- adder (travVar v) (pack $ printPos p )
    return (VarExp v' p)
travExp (CallExp s args p) = do
    args' <- mapM travExp args
    return (CallExp s args' p)
travExp (OpExp l op r p) = do
    l' <- travExp l
    r' <- travExp r
    return (OpExp l' op r' p)
travExp (RecordExp es s p) = do
    es' <- mapM (\(s',e) -> travExp e >>= return . (s',)) es
    return (RecordExp es' s p)
travExp (SeqExp es p) = do
    es' <- mapM travExp es
    return (SeqExp es' p)
travExp (AssignExp v e p) = do
    v' <- adder (travVar v) (pack $ printPos p)
    e' <- travExp e
    return (AssignExp v' e' p)
travExp (IfExp c t Nothing p) = do
    c' <- travExp c
    t' <- travExp t
    return (IfExp c' t' Nothing p)
travExp (IfExp c t (Just e) p) = do
    c' <- travExp c
    t' <- travExp t
    e' <- travExp e
    return (IfExp c' t' (Just e') p)
travExp (WhileExp c b p) = do
    c' <- travExp c
    b' <- travExp b
    return (WhileExp c' b' p)
travExp (ForExp s e lo hi body p) = do
    lo' <- travExp lo
    hi' <- travExp hi
    -- body es analizado en un entorno expandido con s.
    body' <- insert s e (travExp body)
    return (ForExp s e lo' hi' body' p)
travExp (LetExp ds e p) = do
   (ds', e') <- travDecs ds ( do
                                e' <- travExp e
                                ds' <- mapM (\case
                                                (VarDec name _ typ exp p) -> do
                                                  chk <- lookup name
                                                  maybe (internal $ pack $ "666+1 -- Linea:" ++ show p)
                                                        (\(_,esc) -> return (VarDec name esc typ exp p)
                                                           ) chk
                                                l -> return l) ds
                                return (ds', e')
                            )
   return (LetExp ds' e' p)
travExp (ArrayExp typ size init p) = do
    s' <- travExp size
    init' <- travExp init
    return (ArrayExp typ s' init' p)
travExp v = return v

travF :: (Escapator m) => (Symbol,[(Symbol, Escapa , Ty)], Maybe Symbol, Exp, Pos) -> m (Symbol,[(Symbol, Escapa , Ty)], Maybe Symbol, Exp, Pos)
travF (name, params, res, body, p) = do
    (body', params') <- bulkInsert (map (\(a,b,_) -> (a,b)) params) (do
      body' <- travExp body
      ds' <- mapM (\(s,_,ty) -> do
                                mb <- lookup s
                                case mb of
                                    Nothing -> internal $ pack $ "666+2 -- Linea:" ++ show p
                                    Just (_,esc) -> return (s,esc,ty)) params
      return (body', ds'))
    return (name, params', res, body', p)

bulkInsert :: (Escapator m) => [(Symbol, Escapa)] -> m a -> m a
bulkInsert xs m = foldr (\(name, esc) res -> insert name esc res) m xs

travDecs :: (Escapator m) => [Dec] -> m a -> m a
travDecs [] m = m
travDecs ((FunctionDec ls) : xs) m = do
  ls' <- up (mapM travF ls)
  travDecs xs m
travDecs ((VarDec name esc typ init p) : xs) m = do
  init' <- travExp init
  insert name esc (travDecs xs m)
travDecs (l : xs) m = travDecs xs m

-- La profundidad es claramente un simple entero
type Depth = Int

-- El dato que vamos a llevar en el mapa es el nivel en que se definió la variable
-- y si escapa o no.
type Dat = (Int , Escapa)

-- El entorno va a ser simplemente el mapa que lleva la cuenta...
type Env = M.Map Symbol Dat


-- Para mostrar el *poder* del enfoque de usar este sistema de clases
-- voy a definir dos estados, para eventualmente darle dos comportamientos
-- al calculo de escapes.

  -- Este estado va a llevar el nivel y el entorno propiamente dicho
data Estado = S { lvl :: Int, env :: Env}
    deriving Show

-- Este estado va a llevar el nivel y una lista de entornos, que son todos
-- los entorno intermedios...
data SEstado = Step { lvlP :: Int, envP :: Env, msgP :: [String]}
    deriving Show

type Mini = ExceptT Symbol (ST.State Estado)

addMsg :: SEstado -> String -> SEstado
addMsg e msg = e{msgP = msg : msgP e}

instance Demon Mini where
  derror = throwE
  adder w s = catchE w (throwE . flip append s)

instance Escapator Mini where
  depth = lvl <$> get
  up m = do
    old <- get
    put (old{lvl = lvl old + 1})
    m' <- m
    -- put old
    ST.modify (\ (S _ env) -> S (lvl old) env)
    return m'
  update name esc = do
    est <- get
    (lvl, _) <- maybe (notfound name) return (M.lookup name (env est))
    ST.modify (\(S l env) -> S l (M.insert name (lvl, esc) env))
  lookup name = get >>= return . M.lookup name . env
  insert name esc m = do
    old <- get
    put old{env = M.insert name (lvl old, esc) (env old)}
    m' <- m
    new <- get
    if (M.member name (env old))
      then put (new{env = M.insert name ((env old) M.! name) (env new)})
      else put (new{env = M.delete name (env new)})
    return m'
  printEnv = get >>=  \env -> traceM $ "PrintEnv " ++ (show env)

initSt :: Estado
initSt = S 1 M.empty

calcularEEsc :: Exp -> Either Symbol Exp
calcularEEsc e = ST.evalState (runExceptT (travExp e)) initSt
