module TigerProcTransf where

import           TigerProcTransfTypes
import           TigerAssemTypes
import           TigerFrame
import           TigerFCG
import           TigerAbs
--import           TigerGraph
import           TigerSA
import           TigerColoring
import           TigerInterf
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


procTransforms :: (TLGenerator w, ProcTransform w) => w ()
procTransforms = do
  removeDeadAssignments
  regAlloc
  removeDeadAssignments
----------------------------------------
-- Transformaciones
----------------------------------------

replaceInstrs :: [Instr] -> [(Maybe Instr, Int)] -> [Instr]
replaceInstrs ins toReplace = replaceInstrs_ ins toReplace 0
 where
  replaceInstrs_ []  (x : xs) _ = error "Replace Instrs"
  replaceInstrs_ ins []       _ = ins
  replaceInstrs_ (i : is) r@((Nothing, l) : restTR) n
    | l == n    = replaceInstrs_ is restTR (n + 1)
    | otherwise = i : replaceInstrs_ is r (n + 1)
  replaceInstrs_ (i : is) r@((Just ni, l) : restTR) n
    | l == n    = ni : replaceInstrs_ is restTR (n + 1)
    | otherwise = i : replaceInstrs_ is r (n + 1)

removeDeadAssignments :: (ProcTransform w) => w ()
removeDeadAssignments = do
  instrs   <- getInstrs
  liveness <- getLiv
  let labeled         = zip instrs [0 ..] :: [(Instr, Int)]
      assignments     = L.filter isAssignment labeled
      deadAssignments = L.filter (isDeadAssignment liveness) assignments
      unassigned      = L.map (\(_, i) -> (Nothing, i)) deadAssignments
      newInstrs       = replaceInstrs instrs unassigned
  case deadAssignments of
    [] -> return ()
    _  -> setInstrs newInstrs
 where
  isAssignment :: (Instr, Int) -> Bool
  isAssignment (OPER LI{} _ _ _, _) = True
  isAssignment (OPER LA{} _ _ _, _) = True
  isAssignment (OPER LW{} _ _ _, _) = True
  isAssignment (MOV{}          , _) = True
  isAssignment _                    = False
  isDeadAssignment :: M.Map Int (Set.Set Temp) -> (Instr, Int) -> Bool
  isDeadAssignment liv (OPER _ [d] _ _, i) | Set.member d (liv M.! i) = False
                                           | otherwise                = True
  isDeadAssignment liv (MOV _ d _, i) | Set.member d (liv M.! i) = False
                                      | otherwise                = True
  isDeadAssignment _ _ = error "Not an assignment"



---------------------------------------------
-- Funciones para correr las transformaciones
---------------------------------------------

runTransformsASM :: [Instr] -> Frame -> (Frame, [Instr])
runTransformsASM ins f =
  let (_, p) = runState procTransforms (initialProc ins f) in (fr p, instrs p)
