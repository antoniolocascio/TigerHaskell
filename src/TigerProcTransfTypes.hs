module TigerProcTransfTypes where

import           TigerAssemTypes
import           TigerFrame
import           TigerFCG
import           TigerAbs
--import           TigerGraph
import           TigerSA
import           TigerInterf
import           TigerUnique
import           Data.Map                      as M
import           Data.Set                      as Set
import           TigerTemp
import           Data.Graph.Inductive.Graph    as G
import           Data.Graph.Inductive.PatriciaTree
                                               as G
import           Data.List                     as L
import           Control.Arrow
import           TigerSymbol
import           TigerAssem
import           Control.Monad.State

class (Monad w)  => ProcTransform w where
  setInstrs :: [Instr] -> w ()
  getInstrs :: w [Instr]
  getFrame :: w Frame
  setFrame :: Frame -> w ()
  getFCG :: w (FCG Instr)
  getInterf :: w (G.Gr Temp ())
  getNodeMap :: w (M.Map Temp Node)
  getLiv :: w (M.Map Int (Set.Set Temp))
  allocLcl :: w Access
  allocLcl = do
    f <- getFrame
    (nf, acc) <- TigerFrame.allocLocalStack f
    setFrame nf
    return acc

----------------------------------------
-- Instancia de ProcTransform
----------------------------------------

data Proc = Pr {  instrs :: [Instr]
                , fr :: Frame
                , fcg :: FCG Instr
                , interf :: G.Gr Temp ()
                , nodeMap :: M.Map Temp Node
                , liv :: M.Map Int (Set.Set Temp)
                , u :: Integer }

type ProcSt = State Proc

instance ProcTransform ProcSt where
  setInstrs ins = do
    f <- getFrame
    let fcg          = makeFCG ins
        l            = runAnalysis $ newLA fcg
        liveTempSets = L.map (Control.Arrow.second runLD) l
        lvMap        = M.fromList liveTempSets
        tempSets     = getTempsL ins
        int          = makeIG tempSets (snd <$> liveTempSets)
        lnodes = G.labNodes int
        inv    = (\(x, y) -> (y, x)) <$> lnodes
        ndMp = M.fromList inv
        temps = getTempsLNM ins
        maxTmp   = if L.null temps
                      then (-1)
                      else toInteger $ L.maximum (getTempN <$> temps)
    put (Pr ins f fcg int ndMp lvMap maxTmp)
  --getInstrs :: w [Instr]
  getInstrs = gets instrs
  -- getFrame :: w Frame
  getFrame = gets fr
  -- setFrame :: Frame -> w ()
  setFrame f = modify (\p -> p{fr = f})
  -- getFCG :: w FCG
  getFCG = gets fcg
  -- getInterf :: w (G.Gr Temp ())
  getInterf = gets interf
  -- getLiv :: w (M.Map Int (Set.Set Temp))
  getLiv = gets liv
  -- getNodeMap :: w (M.Map Temp Node)
  getNodeMap = gets nodeMap
  -- hasTemps :: w Bool

instance {-# OVERLAPS #-} UniqueGenerator ProcSt where
  mkUnique = modify (\s -> s{u = u s + 1}) >> gets u

initialProc :: [Instr] -> Frame -> Proc
initialProc ins f =
  let fcg          = makeFCG ins
      l            = runAnalysis $ newLA fcg
      liveTempSets = L.map (Control.Arrow.second runLD) l
      lvMap        = M.fromList liveTempSets
      tempSets     = getTempsL ins
      int          = makeIG tempSets (snd <$> liveTempSets)
      lnodes       = G.labNodes int
      inv          = (\(x, y) -> (y, x)) <$> lnodes
      ndMp         = M.fromList inv
      temps        = getTempsLNM ins
      maxTmp       = toInteger $ L.maximum (getTempN <$> temps)
  in  Pr ins f fcg int ndMp lvMap maxTmp
