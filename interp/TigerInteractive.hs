module TigerInteractive where

import           Prelude                 hiding ( (!!) )
import           TigerInterpTypes
import           TigerInterpPP
import           Control.Monad.IO.Class
import           TigerTemp
import qualified Data.List                     as L

import           Data.Map                      as M
                                         hiding ( null )
import           TigerTree
import           TigerInterpFrame
import           TigerSymbol
import           Control.Arrow
import           Control.Monad.State
import           System.IO
import           Debug.Trace
import           System.Exit
import qualified TigerFrame                    as TF


----------------------------------------------------------------------
-- Requiere que todos los argumentos sean pasados por stack
----------------------------------------------------------------------

----------------------------------------
-- Manejo de llamadas externas
----------------------------------------
-- | Función para chequear si es una función externa.
extCall :: Label -> Bool
extCall l = or $ fmap
  ((== l) . pack)
  [ "print"
  , "print_int"
  , "flush"
  , "getchar"
  , "_initArray"
  , "_checkIndexArray"
  , "_checkNil"
  , "_allocRecord"
  , "_stringCompare"
  ]

-- | Llamada externa a print.
-- Dada una dirección de memoria |i : Int| buscamos a que
-- label | mblab : Label | hace referencias y la concatenamos en
-- output (que sería la salida estandard)
printExec :: Int -> RC (Either CPU Int)
printExec i = do
  env <- get
  let mblab = wat env !! i
  put (env { output = output env ++ [getStr mblab] })
  return $ Right 1

printIntExec :: Int -> RC (Either CPU Int)
printIntExec i = do
  env <- get
  put (env { output = output env ++ [pack $ show i] })
  return $ Right 1

newHeapAddr :: RC Int
newHeapAddr = do
  cpu <- get
  let addr = maxHeap cpu
  put cpu { maxHeap = addr + wSz }
  return addr

stringCompare :: Int -> Int -> RC (Either CPU Int)
stringCompare a1 a2 = do
  wats <- gets wat
  let s1 = getStr $ wats !! a1
  let s2 = getStr $ wats !! a2
  case compare s1 s2 of
    Prelude.LT -> return $ Right (-1)
    Prelude.EQ -> return $ Right 0
    Prelude.GT -> return $ Right 1

allocRecord :: [Int] -> RC (Either CPU Int)
allocRecord (ctos : inits) = do
  addr <- newHeapAddr
  cpu  <- get
  let
    new_wat = M.union
      (wat cpu)
      ( M.fromList
      $ L.zipWith (\i v -> (addr + wSz * i, DInt v)) [0 .. (ctos - 1)] inits
      )
  put cpu { wat     = new_wat
          , mem     = M.insert rv addr (mem cpu)
          , maxHeap = addr + wSz * ctos
          }
  return $ Right addr

initArray :: Int -> Int -> RC (Either CPU Int)
initArray size init = do
  addr <- newHeapAddr
  cpu  <- get
  let new_wat = M.union
        (wat cpu)
        (M.fromList $ (addr, DInt size) : L.map
          (\i -> (addr + wSz + wSz * i, DInt init))
          [0 .. (size - 1)]
        )
  put cpu { wat     = new_wat
          , mem     = M.insert rv (addr + wSz) (mem cpu)
          , maxHeap = addr + wSz * (size + 1)
          }
  return $ Right (addr + wSz)

checkIndexArray :: Int -> Int -> RC (Either CPU Int)
checkIndexArray a i = do
  wats   <- gets wat
  cur_fl <- gets (name . head . frameStack)
  cpu    <- get
  if i < 0 || i >= getInt (wats !! (a - wSz))
    then trace ("Indice excedido:" ++ show a ++ " " ++ show i) return (Left cpu)
    else return (Right 0)

checkNil :: Int -> RC (Either CPU Int)
checkNil r = if r == 0
  then do
    cpu <- get
    trace "Nil!\n" return (Left cpu)
  else return $ Right 0

-- | Dispatcher de funciones externas.
extDispatcher :: Label -> [Int] -> RC (Either CPU Int)
extDispatcher l = extDispatcher' (unpack l)
 where
  extDispatcher' "print"            (x     : _)   = printExec x
  extDispatcher' "print_int"        (x     : _)   = printIntExec x
  extDispatcher' "_initArray"       (s : i : _)   = initArray s i
  extDispatcher' "_checkIndexArray" (a : i : _)   = checkIndexArray a i
  extDispatcher' "_checkNil"        (r     : _)   = checkNil r
  extDispatcher' "_allocRecord"     args          = allocRecord args
  extDispatcher' "_stringCompare"   (a1 : a2 : _) = stringCompare a1 a2

----------------------------------------
----------------------------------------

-- | |compute| ejecuta las operaciones aritmeticas básicas.
compute :: BOp -> Int -> Int -> Int
compute Plus  = (+)
compute Minus = (-)
compute Mul   = (*)
compute Div   = div
compute And   = (*)
compute Or    = (+)
compute _     = error "TODO"

-- | |compare| ejecuta las operaciones de relación entre enteros.
compares :: Relop -> Int -> Int -> Bool
compares TigerTree.EQ = (==)
compares NE           = (/=)
compares TigerTree.LT = (<)
compares TigerTree.GT = (>)
compares LE           = (<=)
compares GE           = (>=)
compares _            = error "TODO"

----------------------------------------
----------------------------------------

-- | Cada paso de la máquina es la ejecución de un |Stm| que puede derivar en la
-- necesidad de continuar ejecutando aún más |[Stm]|
step :: Stm -> RC [Stm]
-- Un label no hace nada.
step (Label l              ) = return []
-- Ejecutamos primeo a |l| y dsp |r|
step (Seq  l        r      ) = return [l, r]
-- | Assm load
step (Move (Temp t) (Mem m)) = do
  -- Búscamos que entero representa a |m|
  dir  <- iexp m
  -- Desreferenciamos esa dirección
  wats <- gets wat
  let info = getInt $ wats !! dir
  -- Lo movemos a |t|
  modify $ \env -> env { mem = M.insert t info (mem env) }
  return []
-- El caso general del |Move| (en el que __no__ tenemos que desreferencias memoria),
-- es más sencillo.
step (Move (Temp t) src) = do
  -- Ejecutamos |src|
  val <- iexp src
  -- y movemos el resultado a |t|
  modify $ \env -> env { mem = M.insert t val (mem env) }
  return []
-- | Assm store
-- Igual que antes hacer un |Move| a una memoria
-- es lo mismo que hacer un /Store/ en assembler. Y por ende un cambia el mapa que usamos.
step (Move (Mem t) src) = do
  -- Búscamos que dirección de memoria representa |t|
  dir <- iexp t
  -- ejecutamos |src|
  val <- iexp src
  -- actualizamos la memoria.
  modify $ \env -> env { wat = M.insert dir (DInt val) (wat env) }
  return []
-- Move en el caso que no sea a memoria.
step (Move dst src) = do
  -- Computamos |dst| y |src|
  src' <- iexp src
  dst' <- iexp dst
  -- actualizamos la memoria.
  modify (\env -> env { wat = M.insert dst' (wat env ! src') (wat env) })
  return []
-- Ejecutar una expresión tirando el resultado.
step (ExpS e  ) = iexp e >> return []
-- El |Jump| queda sencillo, es simplemente búscar el código a ejecutar, y devolverlo.
step (Jump _ l) = do
  current_flbl <- gets (name . head . frameStack)
  dats         <- gets dat
  wats         <- gets wat
  let addr           = dats !! current_flbl
  let current_f      = wats !! addr
  let (_, cf_bdy, _) = getFBody current_f
  return $ findLabel cf_bdy l
-- |CJump| es un jump condicional, no creo que lo usen pero es fácil de implementar.
step (CJump bop x y tt ff) = do
  x' <- iexp x
  y' <- iexp y
  let dest = if compares bop x' y' then tt else ff
  current_flbl <- gets (name . head . frameStack)
  dats         <- gets dat
  wats         <- gets wat
  let addr           = dats !! current_flbl
  let current_f      = wats !! addr
  let (_, cf_bdy, _) = getFBody current_f
  return $ findLabel cf_bdy dest

findLabel :: [Stm] -> Label -> [Stm]
findLabel [] _ = error "Label not found"
findLabel (Label l : stms) lbl | l == lbl  = stms
                               | otherwise = findLabel stms lbl
findLabel (s : stms) lbl = findLabel stms lbl

-- | Exp :: TInt
-- Ejecución de expresiones, son instrucciones que retornan un entero
iexp :: Exp -> RC Int
-- | Una expresión |Const i| retorna a |i|
iexp (Const i     ) = return i
-- | |Name l| representa lo que tenga asignado la label |l|, es decir, hay que buscarlo en memoria.
iexp (Name  n     ) = get >>= \e -> return $ dat e !! n
-- | Devolvemos lo que tenga el temporario.
iexp (Temp  t     ) = get >>= \e -> return $ mem e !! t
-- | Computamos la operación |op|, viendo que valor toman los argumentos.
iexp (Binop op x y) = do
  -- Evaluamos a |x|
  x' <- iexp x
  -- Evaluamos a |y|
  y' <- iexp y
  -- Computamos op con |x'| e |y'|
  return $ compute op x' y'
-- | Básicamente desreferenciamos a lo que apunte |e|
iexp (Mem e) = do
  e'  <- iexp e
  env <- get
  return $ getInt $ wat env !! e'
-- | LLamada a función |f| con argumentos |es|.
-- Esto no está implementado totalmente, y posiblemente tampoco correctamente.
iexp (Call (Name f) es) = do
  -- Evaluamos cada uno de los argumentos.
  es' <- mapM iexp es
  cpu <- get
  -- Chequeamos si es externa
  if extCall f
    -- En el caso que sea llamamos al dispatcher.
    then do
      rM <- extDispatcher f es'
      return $ either (error "Error") id rM
    else do
    -- Buscamos la info de |f| cargada en la CPU. Esto nos da un |acc| y el |body|.
      let (acc, body, fr) = getFBody $ getDat f cpu
      -- Deberíamos preparar bien la info de los argumentos, los access de estos
      -- con los argumentos reales que están en |es'|.
      let mvsPreArgs =
            zipWith (\a i -> Move (TigerInterpFrame.exp a 0) (Const i)) acc es'
      let mvsPosArgs = zipWith
            -- Arreglar i aca
            (\a (i, c) -> Move (Temp (TF.argregs L.!! i)) (Const c))
            acc
            (zip [0 .. 3] es')
      let collisions = getCollisions body cpu

      lcls <- parentLocals
      -- Solo los argumentos van a stack, por eso sumo nArgs * wSz
      let mvFP = Move
            (Temp fp)
            (Binop Minus (Temp fp) (Const ((L.length acc + lcls) * wSz)))
      put cpu { frameStack = fr : frameStack cpu }

      interactive (mvFP : mvsPreArgs ++ mvsPosArgs ++ body)

      modify (\c -> c { frameStack = tail (frameStack c) })
      cpu' <- get
      restore collisions
      -- Buscar el resultado en rv y devolverlo.
      return $ mem cpu' !! rv
-- En ppio no puede pasar otra cosa. A menos que estemos en un leng funcional ;)
iexp (Call _ _) = error "Puede pasar?"
-- | |Eseq| es la ejecución secuencial de los pasos.
iexp (Eseq s e) = step s >> iexp e

parentLocals :: RC Int
parentLocals = do
  frames <- gets frameStack
  case frames of
    []      -> error "No tiene padre"
    (f : _) -> return $ L.length (locals f)


getCollisions :: [Stm] -> CPU -> [(Temp, Int)]
getCollisions stms cpu =
  let temps   = getTemps stms
      defined = M.keys (mem cpu)
      colls   = L.intersect temps defined
  in  L.map (\t -> (t, mem cpu !! t)) colls
 where
  getTemps :: [Stm] -> [Temp]
  getTemps [] = [sp, fp] ++ TF.argregs
  getTemps (Move (Temp t) _ : stms) =
    if t == rv then getTemps stms else t : getTemps stms
  getTemps (Seq s1 s2 : stms) = getTemps $ s1 : s2 : stms
  getTemps (_         : stms) = getTemps stms

restore :: [(Temp, Int)] -> RC ()
restore colls = do
  cpu <- get
  put cpu { mem = M.union (M.fromList colls) (mem cpu) }

--------------------------------------------------------------------------------
-- Definiciones para que la CPU corra.
--------------------------------------------------------------------------------

-- | Estado inicial de la CPU.
-- fp, sp, rv = 0.
emptyCPU :: CPU
emptyCPU = CPU M.empty M.empty M.empty [] [] [] True 0

emptyCPUNI :: CPU
emptyCPUNI = CPU M.empty M.empty M.empty [] [] [] False 0

-- | Función que búsca los posibles labels dentro de una sequencia de stms.
splitStms
  :: [Stm]
  ->
          -- | Lista de stms hasta encontrar un Label.
     ([Stm]
          -- | Segmentos Lable lista de Stmts debajo de él.
           , [(Label, [Stm])])
splitStms []             = ([], [])
splitStms (Label l : ts) = ([], splitLbls ts (l, []))
splitStms (t       : ts) = let (res, lbls) = splitStms ts in (t : res, lbls)

-- | Función auxiliar que claramente hace todo el trabajo. Básicamente va
-- acumulando hasta encontrar un Label, lo agrega al final de la lista, y pasa a
-- acumular otro label.
splitLbls :: [Stm] -> (Label, [Stm]) -> [(Label, [Stm])]
splitLbls []             ts      = [second reverse ts]
splitLbls (Label l : ts) rs      = second reverse rs : splitLbls ts (l, [])
splitLbls (t       : ts) (l, rs) = splitLbls ts (l, t : rs)


----------------------------------------
-- | Función que genera una nueva dirección de memoria. Lo usamos
-- para la asignación de direcciones a nombres en la CPU.
newDir :: State Int Int
newDir = do
  i <- get
  put (i - 1)
  return i

loadLabels :: [(Label, Symbol)] -> State Int CPU -> State Int CPU
loadLabels []                  st = st
loadLabels ((lbl, sym) : defs) st = do
  st' <- st
  dir <- newDir
  loadLabels defs $ return
    (st' { dat = M.insert lbl dir (dat st')
         , wat = M.insert dir (Str sym) (wat st')
         }
    )

loadLabCod :: [(Label, [Stm])] -> State Int CPU -> State Int CPU
loadLabCod []                 cpu = cpu
loadLabCod ((lbl, cod) : res) cpu = do
  st' <- cpu
  dir <- newDir
  loadLabCod res $ return
    (st' { dat = M.insert lbl dir (dat st')
         , wat = M.insert dir (FBody ([], cod, defaultFrame)) (wat st')
         }
    )

loadProcs :: [(Frame, [Stm])] -> State Int CPU -> State Int CPU
loadProcs []                    cpu = cpu
loadProcs ((fr, fbody) : procs) cpu = do
  let fname             = name fr
      (factBody, rests) = splitStms fbody
  fdir <- newDir
  cpu' <- cpu
  loadProcs procs $ return
    (cpu' { dat = M.insert fname fdir (dat cpu')
          , wat = M.insert fdir (FBody (prepFormals fr, fbody, fr)) (wat cpu')
          }
    )

loadMain :: [Stm] -> Frame -> State Int CPU -> State Int CPU
loadMain tmain fr s = do
  cpu  <- s
  fdir <- newDir
  return $ cpu { dat = M.insert (pack "_tigermain_0_0") fdir (dat cpu)
               , wat = M.insert fdir (FBody ([], tmain, defaultFrame)) (wat cpu)
               , frameStack = [fr]
               }

----------------------------------------
----------------------------------------
-- | Inicializador interactivo
startInteractive
  :: 
        -- | Fragmentos de funciones ya definidas. (fuera del main)
     [(Frame, [Stm])]
        -- | Strings.
  -> [(Label, Symbol)]
        -- | Básicamente el main.
  -> ([Stm], Frame)
  -> IO (Int, [Symbol])
startInteractive fs ss (tmain, mainFr) =
  let (factMain, rests) = splitStms tmain
      (cpuInit', me   ) = runState
        ( loadMain tmain mainFr
        $ loadLabCod rests
        $ loadProcs fs
        $ loadLabels ss
        $ return emptyCPU
        )
        memSize
      cpuInit   = uTemp rv me $ uTemp fp me $ uTemp sp me cpuInit'
      factMain' = if null factMain then snd $ head rests else factMain
  in  evalStateT (interactive tmain) cpuInit

-- | Inicializador no interactivo
startInterp
  :: 
        -- | Fragmentos de funciones ya definidas. (fuera del main)
     [(Frame, [Stm])]
        -- | Strings.
  -> [(Label, Symbol)]
        -- | Básicamente el main.
  -> ([Stm], Frame)
  -> IO (Int, [Symbol])
startInterp fs ss (tmain, mainFr) =
  let (factMain, rests) = splitStms tmain
      (cpuInit', me   ) = runState
        ( loadMain tmain mainFr
        $ loadLabCod rests
        $ loadProcs fs
        $ loadLabels ss
        $ return emptyCPUNI
        )
        memSize
      cpuInit   = uTemp rv me $ uTemp fp me $ uTemp sp me cpuInit'
      factMain' = if null factMain then snd $ head rests else factMain
  in  evalStateT (interactive tmain) cpuInit


interactive :: [Stm] -> RC (Int, [Symbol])
interactive [] = do
  cpu <- get
  case frameStack cpu of
    [f] ->
      liftIO $ putStrLn (printCpu cpu) >> return (mem cpu ! rv, output cpu)
    _ -> return (0, [])
interactive s@(stm : stms) = do
  cpu <- get
  ln  <- if inter cpu
    then do
      liftIO $ putStrLn ("Next stm: " ++ renderStm stm)
      liftIO (putStr "> " >> hFlush stdout)
      liftIO getLine
    else return "c"
  case ln of
    "c" -> do
      stms' <- step stm
      case stm of
        (Jump _ _) -> interactive stms'
        _          -> interactive (stms' ++ stms)
    "s" -> liftIO (putStrLn (renderCPU cpu)) >> interactive s
    "m" -> liftIO (putStrLn (renderMem $ mem cpu)) >> interactive s
    "w" -> liftIO (putStrLn (renderWat $ wat cpu)) >> interactive s
    "d" -> liftIO (putStrLn (renderDat $ dat cpu)) >> interactive s
    "r" -> put cpu { inter = False } >> interactive s
    "n" -> do
      liftIO (putStrLn (renderCode stms))
      interactive s
    "q" -> liftIO exitSuccess
    "h" -> liftIO (putStrLn help) >> interactive s
    _   -> liftIO (putStrLn "Comando desconocido") >> interactive s


help :: String
help =
  "Comandos:\n"
    ++ "\tc: Ejecutar un paso\n"
    ++ "\ts: Mostrar el estado de la CPU\n"
    ++ "\tm: Mostrar estado de Mem\n"
    ++ "\tw: Mostrar estado de Wat\n"
    ++ "\td: Mostrar estado de Dat\n"
    ++ "\tr: Ejecutar el resto del programa\n"
    ++ "\tn: Mostrar el codigo restante\n"
    ++ "\tq: Salir\n"
