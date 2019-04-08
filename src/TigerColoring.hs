module TigerColoring where

import           Prelude                 hiding ( (!!) )
import           TigerProcTransfTypes
import           TigerAssemTypes
import           Control.Monad.State
import           TigerFrame
import           TigerAbs
import           TigerAssem
import           TigerSA
import           TigerInterf
import           Data.Map                      as M
import           Data.Set                      as Set
import           TigerTemp
import           Data.Graph.Inductive.Graph    as G
import           Data.Graph.Inductive.PatriciaTree
                                               as G
import           Data.List                     as L
                                         hiding ( (!!) )
import           TigerSymbol
import           TigerAssem
import           Data.Maybe
import           Control.Monad
import           Control.Conditional     hiding ( when )
import           Debug.Trace
import           Control.Arrow

data MoveClass = Coalesced | Worklist | Active | Constrained | Frozen deriving (Show, Ord, Eq)

type Line = Int

data WorkStructures = WS {  simplifyWorklist :: Set Node
                          , freezeWorklist :: Set Node
                          , spillWorklist :: Set Node
                          , spilledNodes :: Set Node
                          , coalescedNodes :: Set Node
                          , coloredNodes :: Set Node
                          , selectStack :: [Node]
                          --
                          , code :: [(Line, Instr)]
                          , graph :: G.Gr Temp ()
                          , nodeM :: M.Map Node Temp
                          , tempM :: M.Map Temp Node
                          , moveList :: M.Map Node [Line]
                          , moveClasses :: M.Map Line MoveClass
                          , alias :: M.Map Node Node
                          , color :: M.Map Node Temp
                         } deriving Show

type WSSt = State WorkStructures

-- Map helper test function
(!!) :: (Ord k, Show k, Show v) => Map k v -> k -> v
m !! k =
  maybe (error $ "No encontrada la clave: " ++ show k ++ " in: " ++ show m) id
    $ M.lookup k m

colors :: [Temp]
colors = callersaves ++ calleesaves ++ argregs

k :: Int
k = L.length colors

precolored :: Temp -> Bool
precolored = ('T' /=) . head . unpack

precoloredN :: Node -> WSSt Bool
precoloredN = fmap precolored . getTemp

degr :: Node -> WSSt Int
degr n = do
  gr <- gets graph
  return $ G.deg gr n

adj :: Node -> Node -> WSSt Bool
adj n1 n2 = do
  gr <- gets graph
  return $ G.hasNeighbor gr n1 n2

getTemp :: Node -> WSSt Temp
getTemp n = do
  ndMp <- gets nodeM
  return (ndMp !! n)

activeOrWorklist :: M.Map Int MoveClass -> Int -> Bool
activeOrWorklist m i = let c = m !! i in c == Active || c == Worklist

nodeMoves :: Node -> WSSt [Line]
nodeMoves n = do
  mL   <- gets moveList
  mCls <- gets moveClasses
  let mvs = mL !! n
  return $ L.filter (activeOrWorklist mCls) mvs

moveRelated :: Node -> WSSt Bool
moveRelated n = do
  mvs <- nodeMoves n
  return $ not (L.null mvs)

removeFromSpill :: Node -> WSSt ()
removeFromSpill n =
  modify (\s -> s { spillWorklist = spillWorklist s Set.\\ Set.singleton n })

removeFromFreeze :: Node -> WSSt ()
removeFromFreeze n =
  modify (\s -> s { freezeWorklist = freezeWorklist s Set.\\ Set.singleton n })

addToFreeze :: Node -> WSSt ()
addToFreeze n = modify
  (\s -> s { freezeWorklist = freezeWorklist s `Set.union` Set.singleton n })

addToSimpl :: Node -> WSSt ()
addToSimpl n = modify
  (\s -> s { simplifyWorklist = simplifyWorklist s `Set.union` Set.singleton n }
  )

addToCoalesced :: Node -> WSSt ()
addToCoalesced n = modify
  (\s -> s { coalescedNodes = coalescedNodes s `Set.union` Set.singleton n })

addToSpillWl :: Node -> WSSt ()
addToSpillWl n = modify
  (\s -> s { spillWorklist = spillWorklist s `Set.union` Set.singleton n })

addToSpilled :: Node -> WSSt ()
addToSpilled n =
  modify (\s -> s { spilledNodes = spilledNodes s `Set.union` Set.singleton n })

----------------------------------------
-- Simplify
----------------------------------------
simplify :: WSSt ()
simplify = do
  simpl <- gets simplifyWorklist
  stack <- gets selectStack
  gr    <- gets graph
  let n         = Set.elemAt 0 simpl
      new_simpl = Set.deleteAt 0 simpl
      new_gr    = G.delNode n gr
  pc_n <- precoloredN n
  let new_stack = if pc_n then stack else n : stack
  modify
    (\s -> s { simplifyWorklist = new_simpl
             , selectStack      = new_stack
             , graph            = new_gr
             }
    )
  mapM_ decrementDegree (G.neighbors gr n)

decrementDegree :: Node -> WSSt ()
decrementDegree n = do
  gr <- gets graph
  let d = G.deg gr n
  when
    (d == k)
    (do
      enableMoves (Set.insert n (Set.fromList $ G.neighbors gr n))
      removeFromSpill n
      ifM (moveRelated n) (addToFreeze n) (addToSimpl n)
    )

enableMoves :: Set Node -> WSSt ()
enableMoves = mapM_ enableMove
 where
  enableMove :: Node -> WSSt ()
  enableMove n = do
    mvs <- nodeMoves n
    modify (\s -> s { moveClasses = activeToWorklist mvs (moveClasses s) })
  activeToWorklist :: [Line] -> M.Map Line MoveClass -> M.Map Line MoveClass
  activeToWorklist lns m =
    let activeLns = L.filter (\ln -> m !! ln == Active) lns
        newVals   = L.zip activeLns (L.repeat Worklist)
    in  M.union m (M.fromList newVals)


----------------------------------------
-- Coalesce
----------------------------------------
coalesce :: WSSt ()
coalesce = do
  ln              <- getMoveLine
  (MOV _ dst src) <- getMove ln
  ndst            <- getNodeFromTemp dst
  nsrc            <- getNodeFromTemp src
  dst_a           <- getAlias ndst
  src_a           <- getAlias nsrc
  let (u, v) = if precolored dst then (src_a, dst_a) else (dst_a, src_a)
  tu <- getTemp u
  tv <- getTemp v
  gr <- gets graph
  if u == v
    then do
      setMove ln Coalesced
      addWorklist u
    else if precolored tv || G.hasNeighbor gr u v
      then do
        setMove ln Constrained
        addWorklist u
        addWorklist v
      else do
        let u_pc   = precolored tu
            adjs_v = G.neighbors gr v
            adjs_u = G.neighbors gr u
        all_adj_ok <- fmap L.and (mapM (`ok` u) adjs_v)
        cons       <- tory $ L.union adjs_v adjs_u
        if (u_pc && all_adj_ok) || (not u_pc && cons)
          then do
            setMove ln Coalesced
            combine u v
            addWorklist u
          else setMove ln Active

getMoveLine :: WSSt Line
getMoveLine = do
  mCls <- gets moveClasses
  let mvLs = M.toList mCls
  return $ getWorklistMove mvLs
 where
  getWorklistMove [] = error "getMoveLine: worklistMoves vacia"
  getWorklistMove ((ln, Worklist) : _) = ln
  getWorklistMove (_ : rest) = getWorklistMove rest

getMove :: Line -> WSSt Instr
getMove ln = do
  cd <- gets code
  case L.lookup ln cd of
    Nothing    -> error "getMove: No se encuentra la linea"
    Just instr -> return instr

getAlias :: Node -> WSSt Node
getAlias n = do
  coalescedNs <- gets coalescedNodes
  if n `Set.member` coalescedNs
    then do
      als <- gets alias
      getAlias (als !! n)
    else return n

setAlias :: Node -> Node -> WSSt ()
setAlias v u = modify (\s -> s { alias = M.insert v u (alias s) })

getNodeFromTemp :: Temp -> WSSt Node
getNodeFromTemp t = do
  ndMp <- gets tempM
  return $ ndMp !! t

addWorklist :: Node -> WSSt ()
addWorklist u = do
  pc <- precoloredN u
  mr <- moveRelated u
  d  <- degr u
  when (not pc && not mr && d < k) (removeFromFreeze u >> addToSimpl u)

setMove :: Line -> MoveClass -> WSSt ()
setMove ln cl =
  modify (\s -> s { moveClasses = M.insert ln cl (moveClasses s) })

ok :: Node -> Node -> WSSt Bool
ok t r = do
  d_t   <- degr t
  pc_t  <- precoloredN t
  nhbrs <- adj t r
  return $ d_t < k || pc_t || nhbrs

tory :: [Node] -> WSSt Bool
tory nodes = do
  gr <- gets graph
  let n = L.length $ L.filter (significant gr) nodes
  return $ n < k
 where
  significant :: G.Gr Temp () -> Node -> Bool
  significant gr n = G.deg gr n >= k

combine :: Node -> Node -> WSSt ()
combine u v = do
  freezeWl <- gets freezeWorklist
  if v `Set.member` freezeWl then removeFromFreeze v else removeFromSpill v
  addToCoalesced v
  setAlias v u
  mergeMoves u v
  addNewEdges u v
  d_u       <- degr u
  freezeWl' <- gets freezeWorklist
  when
    (d_u >= k && u `Set.member` freezeWl')
    (do
      removeFromFreeze u
      addToSpillWl u
    )
 where
  mergeMoves :: Node -> Node -> WSSt ()
  mergeMoves u v = modify
    (\s ->
      let prev_ml  = moveList s
          new_ml_u = L.union (prev_ml !! v) (prev_ml !! u)
      in  s { moveList = M.insert u new_ml_u prev_ml }
    )
  addNewEdges :: Node -> Node -> WSSt ()
  addNewEdges u v = do
    gr <- gets graph
    let adj_v = G.neighbors gr v
    new_gr <- foldM
      (\g t -> decrementDegree t >> return (G.insEdge (t, u, ()) g))
      gr
      adj_v
    modify (\s -> s { graph = new_gr })


----------------------------------------
-- Freeze
----------------------------------------
freeze :: WSSt ()
freeze = do
  freezeWl <- gets freezeWorklist
  let u = Set.elemAt 0 freezeWl
  removeFromFreeze u
  addToSimpl u
  freezeMoves u

freezeMoves :: Node -> WSSt ()
freezeMoves u = do
  mvs_u <- nodeMoves u
  mapM_ freezeMove mvs_u
 where
  freezeMove :: Line -> WSSt ()
  freezeMove ln = do
    (MOV _ dst src) <- getMove ln
    ndst            <- getNodeFromTemp dst
    nsrc            <- getNodeFromTemp src
    dst_a           <- getAlias ndst
    src_a           <- getAlias nsrc
    u_alias         <- getAlias u
    let v = if src_a == u_alias then dst_a else src_a
    setMove ln Frozen
    mvs_v <- nodeMoves v
    d_v   <- degr v
    when
      (L.null mvs_v && d_v < k)
      (do
        removeFromFreeze v
        addToSimpl v
      )

----------------------------------------
-- Select Spill
----------------------------------------
selectSpill :: WSSt ()
selectSpill = do
  spllWl    <- gets spillWorklist
  tmpSpllWl <- mapM (\n -> (n, ) <$> getTemp n) (Set.toList spllWl)
  let (n, _) = L.minimumBy
        (\(_, t1) (_, t2) -> compare (getTempN t1) (getTempN t2))
        tmpSpllWl
  removeFromSpill n
  addToSimpl n
  freezeMoves n



----------------------------------------
-- Assign Colors
----------------------------------------
assignColors :: G.Gr Temp () -> WSSt ()
assignColors gr = do
  acWhile gr
  coalesced <- gets coalescedNodes
  cTmps     <- mapM getTemp (Set.toList coalesced)
  mapM_ setAliasColor coalesced
 where
  setAliasColor :: Node -> WSSt ()
  setAliasColor n = do
    a_n <- getAlias n
    a_c <- getColorM a_n
    maybe (return ()) (setColor n) a_c

acWhile :: G.Gr Temp () -> WSSt ()
acWhile gr = do
  slct <- gets selectStack
  Control.Monad.unless
    (L.null slct)
    (do
      n          <- popFromSelect
      neigh_cols <- getNeighborsColors gr n
      let ok_cols = colors L.\\ neigh_cols
      if L.null ok_cols
        then addToSpilled n
        else do
          addToColored n
          setColor n (L.head ok_cols)
      acWhile gr
    )

getNeighborsColors :: G.Gr Temp () -> Node -> WSSt [Temp]
getNeighborsColors gr n = do
  let neighbors = G.neighbors gr n
  neighbors_colors <- mapM getColorM neighbors
  return $ removeDups (catMaybes neighbors_colors)
 where
  removeDups :: (Ord a) => [a] -> [a]
  removeDups = L.map L.head . L.group . L.sort

getColorM :: Node -> WSSt (Maybe Temp)
getColorM n = do
  a_n <- getAlias n
  a_t <- getTemp a_n
  if precolored a_t
    then return (Just a_t)
    else do
      cl <- gets color
      return $ M.lookup a_n cl

popFromSelect :: WSSt Node
popFromSelect = do
  slct <- gets selectStack
  modify (\s -> s { selectStack = tail slct })
  return $ head slct

setColor :: Node -> Temp -> WSSt ()
setColor n c = modify (\s -> s { color = M.insert n c (color s) })

addToColored :: Node -> WSSt ()
addToColored n =
  modify (\s -> s { coloredNodes = Set.insert n (coloredNodes s) })

----------------------------------------
-- Rewrite Program
----------------------------------------
data TempKind = Use | Def | UseDef deriving (Eq, Show)

rewriteProgram :: (TLGenerator w, ProcTransform w) => [Temp] -> w ()
rewriteProgram spilledTmps = do
  cod  <- getInstrs
  accs <- mapM (const allocLcl) spilledTmps
  let accM  = M.fromList $ L.zip spilledTmps accs
      spSet = Set.fromList spilledTmps
  new_cod <- replaceSpilledTmps cod accM spSet
  setInstrs new_cod
 where
  fetch :: Temp -> Access -> Instr
  fetch t (InFrame ofst)
    | inHalfRange ofst = OPER
      { assem = LW t fp ofst
      , srcs  = [fp]
      , dsts  = [t]
      , jump  = Nothing
      }
    | otherwise = error "fetch: Ofst no entra en halfword"
  store :: Temp -> Access -> Instr
  store t (InFrame ofst)
    | inHalfRange ofst = OPER
      { assem = SW t ofst fp
      , srcs  = [fp, t]
      , dsts  = []
      , jump  = Nothing
      }
    | otherwise = error "store: Ofst no entra en halfword"

  replaceSpilledTmps
    :: (TLGenerator w, Monad w)
    => [Instr]
    -> M.Map Temp Access
    -> Set.Set Temp
    -> w [Instr]
  replaceSpilledTmps []           _    _     = return []
  replaceSpilledTmps (ins : rest) accM spSet = do
    rest_cod <- replaceSpilledTmps rest accM spSet
    let spilled = spilledInInstr ins spSet
    if L.null spilled
      then return $ ins : rest_cod
      else do
        newTmps <- mapM (const newTemp) spilled
        let
          tmpM    = M.fromList $ L.zip (fst <$> spilled) newTmps
          toFetch = fst <$> L.filter ((/= Def) . snd) spilled
          toStore = fst <$> L.filter ((/= Use) . snd) spilled
          fetches = L.map (\t -> fetch (tmpM !! t) (accM !! t)) toFetch
          stores  = L.map (\t -> store (tmpM !! t) (accM !! t)) toStore
          new_ins = L.foldr
            (\(ot, _) i -> replaceTempInstr i ot (tmpM !! ot))
            ins
            spilled
        return $ fetches ++ [new_ins] ++ stores ++ rest_cod

  spilledInInstr :: Instr -> Set.Set Temp -> [(Temp, TempKind)]
  spilledInInstr LABEL{} spilled = []
  spilledInInstr (MOV _ d s) spilled =
    getSpilled [(d, Def), (s, Use)] [] spilled
  spilledInInstr (OPER _ ds ss _) spilled =
    getSpilled (L.zip ds (L.repeat Def) ++ L.zip ss (L.repeat Use)) [] spilled
  getSpilled
    :: [(Temp, TempKind)]
    -> [(Temp, TempKind)]
    -> Set.Set Temp
    -> [(Temp, TempKind)]
  getSpilled [] ac _ = ac
  getSpilled ((t, tk) : rest) ac spilled
    | Set.member t spilled = case L.lookup t ac of
      Nothing     -> getSpilled rest ((t, tk) : ac) spilled
      Just UseDef -> getSpilled rest ac spilled
      Just tk'
        | tk' == tk -> getSpilled rest ac spilled
        | tk' /= tk -> getSpilled rest
                                  ((t, UseDef) : L.filter ((/= t) . fst) ac)
                                  spilled
    | otherwise = getSpilled rest ac spilled

applyCoalescing :: WSSt [Instr]
applyCoalescing = do
  mCls <- gets moveClasses
  let coalescedMs = M.keys $ M.filter (== Coalesced) mCls
  cod <- gets code
  let coalesced_code = snd <$> L.filter (\(ln, _) -> L.elem ln coalescedMs) cod
  let cod_wo_coalesced_moves =
        snd <$> L.filter (\(ln, _) -> L.notElem ln coalescedMs) cod
  to_repl <- getTempsToRepl
  return $ rmEqSDMvs (L.map (setColors to_repl) cod_wo_coalesced_moves)
 where
  rmEqSDMvs :: [Instr] -> [Instr]
  rmEqSDMvs [] = []
  rmEqSDMvs (m@(MOV _ s d) : rest) | s == d    = rmEqSDMvs rest
                                   | otherwise = m : rmEqSDMvs rest
  rmEqSDMvs (i : rest) = i : rmEqSDMvs rest

  getTempsToRepl :: WSSt [(Temp, Temp)]
  getTempsToRepl = do
    coal_nodes <- gets coalescedNodes
    aliases    <- mapM getAlias (Set.toList coal_nodes)
    zipWithM
      (\n a ->
        (do
          tn <- getTemp n
          ta <- getTemp a
          return (tn, ta)
        )
      )
      (Set.toList coal_nodes)
      aliases

applyColoring :: [Instr] -> M.Map Node Temp -> M.Map Node Temp -> [Instr]
applyColoring cod ndMp col =
  let col_list = M.toList col
      to_rpl   = L.map (Control.Arrow.first (ndMp !!)) col_list
  in  rmEqSDMvs $ L.map (setColors to_rpl) cod
 where
  rmEqSDMvs :: [Instr] -> [Instr]
  rmEqSDMvs [] = []
  rmEqSDMvs (m@(MOV _ s d) : rest) | s == d    = rmEqSDMvs rest
                                   | otherwise = m : rmEqSDMvs rest
  rmEqSDMvs (i : rest) = i : rmEqSDMvs rest

setColors :: [(Temp, Temp)] -> Instr -> Instr
setColors to_rpl instr =
  L.foldr (\(ot, nt) i -> replaceTempInstr i ot nt) instr to_rpl

----------------------------------------
-- Main
----------------------------------------
-- Setear todo menos las listas de nodos
makeWS1 :: ProcTransform w => w ([Node], WorkStructures)
makeWS1 = do
  instrs <- getInstrs
  gr     <- getInterf
  ndMp   <- getNodeMap
  let cod      = L.zip ([0 ..] :: [Line]) instrs
      temps    = getTempsLNM instrs
      initial  = getNodes ndMp temps
      mvs      = L.filter (\(_, i) -> isMove i) cod
      mCls     = M.fromList $ L.zip (fst <$> mvs) (repeat Worklist)
      nodes    = G.nodes gr
      initMvLs = M.fromList (zip nodes (repeat []))
      mvLs     = makeMoveList (makeNodePairs mvs ndMp) ndMp initMvLs
      ndMp_inv = M.fromList $ (\(x, y) -> (y, x)) <$> M.toList ndMp
      ws       = WS
        { simplifyWorklist = Set.empty
        , freezeWorklist   = Set.empty
        , spillWorklist    = Set.empty
        , spilledNodes     = Set.empty
        , coalescedNodes   = Set.empty
        , coloredNodes     = Set.empty
        , selectStack      = []
        , code             = cod
        , graph            = gr
        , nodeM            = ndMp_inv
        , tempM            = ndMp
        , moveList         = mvLs
        , moveClasses      = mCls
        , alias            = M.empty
        , color            = M.empty
        }
  return (initial, ws)
 where
  getNodes :: M.Map Temp Node -> [Temp] -> [Node]
  getNodes m = L.map (m !!)
  isMove :: Instr -> Bool
  isMove MOV{} = True
  isMove _     = False
  makeMoveList
    :: [(Line, (Node, Node))]
    -> M.Map Temp Node
    -> M.Map Node [Line]
    -> M.Map Node [Line]
  makeMoveList [] ndMp i = i
  makeMoveList ((ln, (d, s)) : rest) ndMp i =
    let prevM     = makeMoveList rest ndMp i
        prevLns_d = safeLookup prevM d
        prevLns_s = safeLookup prevM s
    in  M.insert d
                 (prevLns_d `L.union` [ln])
                 (M.insert s (prevLns_s `L.union` [ln]) prevM)
  safeLookup :: M.Map Node [Line] -> Node -> [Line]
  safeLookup m n = fromMaybe [] (M.lookup n m)
  makeNodePairs :: [(Line, Instr)] -> M.Map Temp Node -> [(Line, (Node, Node))]
  makeNodePairs l ndMp =
    L.map (\(ln, MOV _ dst src) -> (ln, (ndMp !! dst, ndMp !! src))) l

-- Setear worklists de nodos (MakeWorklist())
makeWS2 :: [Node] -> WSSt ()
makeWS2 []         = return ()
makeWS2 (n : rest) = do
  d_n <- degr n
  if d_n >= k
    then addToSpillWl n
    else do
      mr_d <- moveRelated n
      if mr_d then addToFreeze n else addToSimpl n
  makeWS2 rest

mainLoop :: WSSt ()
mainLoop = do
  mainLoopBody
  simplWl      <- gets simplifyWorklist
  emptyWlMoves <- isWlMovesEmpty
  freezeWl     <- gets freezeWorklist
  spillWl      <- gets spillWorklist
  Control.Monad.unless
    (Set.null simplWl && emptyWlMoves && Set.null freezeWl && Set.null spillWl)
    mainLoop

mainLoopBody :: WSSt ()
mainLoopBody = do
  simplWl <- gets simplifyWorklist
  if not (Set.null simplWl)
    then simplify
    else do
      emptyWlMoves <- isWlMovesEmpty
      if not emptyWlMoves
        then coalesce
        else do
          freezeWl <- gets freezeWorklist
          if not (Set.null freezeWl)
            then freeze
            else do
              spillWl <- gets spillWorklist
              Control.Monad.unless (Set.null spillWl) selectSpill

isWlMovesEmpty :: WSSt Bool
isWlMovesEmpty = do
  mCls <- gets moveClasses
  let worklist = M.filter (== Worklist) mCls
  return $ M.null worklist

regAlloc :: (TLGenerator w, ProcTransform w) => w ()
regAlloc = do
  (initial, ws1) <- makeWS1
  gr             <- getInterf
  let (new_code, ws2) = runState
        (makeWS2 initial >> mainLoop >> assignColors gr >> applyCoalescing)
        ws1
  setInstrs new_code
  let spilledNs   = Set.toList $ spilledNodes ws2
      ndMp        = nodeM ws2
      spilledTmps = (ndMp !!) <$> spilledNs
  if L.null spilledNs
    then setInstrs $ applyColoring new_code (nodeM ws2) (color ws2)
    else rewriteProgram spilledTmps >> regAlloc
