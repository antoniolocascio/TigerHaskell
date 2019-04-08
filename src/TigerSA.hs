{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TigerSA where

import           TigerAssemTypes
import           TigerFCG
import qualified Data.Set                      as Set
import           TigerTemp
import           Data.Graph.Inductive.Graph    as G
import           Data.List                     as L
                                         hiding ( union )
import qualified Data.List                     as L
                                                ( union )

class Domain d where
  before :: Ord a => d a -> d a -> Bool
  sup :: Ord a => d a -> d a -> d a
  bottom :: d a
  union :: Ord a => d a -> d a -> d a
  diff :: Ord a => d a -> d a -> d a
  eq :: (Ord a, Eq a) => d a -> d a -> Bool

data Lattice d a where
  L :: Domain d => {domain :: d a} -> Lattice d a

data Program d a b where
   P :: Domain d => {  ext :: Int
                     , tau :: d a
                     , kill :: b -> d a
                     , gen  :: b -> d a
                     , flow :: Int -> [Int]
                     , flow_r :: Int -> [Int]
                     , instr :: Int -> b
                     , n :: Int } -> Program d a b

data StaticAnalysis d a b where
  SA :: Domain d => {l :: Lattice d a, p :: Program d a b} -> StaticAnalysis d a b

runAnalysis
  :: forall d a b . (Domain d, Ord a) => StaticAnalysis d a b -> [(Int, d a)]
runAnalysis s =
  let initial  = (map (\i -> (i, bottom :: d a)) [0 .. (n (p s))])
      worklist = [0 .. (n (p s))]
  in  worklistAn s initial worklist --analysis s initial

worklistAn
  :: (Domain d, Ord a)
  => StaticAnalysis d a b
  -> [(Int, d a)] -- analysis
  -> [Int]        -- worklist
  -> [(Int, d a)]
worklistAn s an [] = an
worklistAn s an (l : wl) =
  let (_, newVal) = proj s an l
  in  if eq newVal (snd $ an L.!! l)
        then worklistAn s an wl
        else
          let newAn = L.take l an ++ [(l, newVal)] ++ L.drop (l + 1) an
              newWl = L.union wl (flow (p s) l)
          in  worklistAn s newAn newWl

analysis
  :: (Domain d, Ord a) => StaticAnalysis d a b -> [(Int, d a)] -> [(Int, d a)]
analysis s prev =
  let next = transformer s prev
  in  if L.all (\((ia, da), (ib, db)) -> ia == ib && eq da db) (zip next prev)
        then next
        else analysis s next

transformer
  :: (Domain d, Ord a) => StaticAnalysis d a b -> [(Int, d a)] -> [(Int, d a)]
transformer s ds = map (\(i, _) -> proj s ds i) ds

proj
  :: (Domain d, Ord a)
  => StaticAnalysis d a b
  -> [(Int, d a)]
  -> Int
  -> (Int, d a)
proj s ds i
  | i == ext (p s)
  = (i, tau (p s))
  | otherwise
  = let js   = (flow_r (p s)) i
        f_js = map (\j -> f s j (snd $ ds !! j)) js
    in  (i, foldr sup bottom f_js)

f :: (Domain d, Ord a) => StaticAnalysis d a b -> Int -> d a -> d a
f s l d =
  union (gen (p s) (instr (p s) l)) (diff d (kill (p s) (instr (p s) l)))


----------------------------------------
-- Liveness Analysis en Instrucciones
----------------------------------------

newtype LivenessDomain a = LD {runLD :: Set.Set a} deriving (Eq, Show)

instance Domain LivenessDomain where
  before d1 d2 = Set.isSubsetOf (runLD d1) (runLD d2)
  sup d1 d2 = LD $ Set.union (runLD d1) (runLD d2)
  bottom = LD Set.empty
  union = sup
  diff d1 d2 = LD $ Set.difference (runLD d1) (runLD d2)
  eq = (==)

type LivenessLattice = Lattice LivenessDomain Temp

newLL :: Set.Set Temp -> LivenessLattice
newLL dom = L (LD dom)

type LivenessProgram  = Program LivenessDomain Temp Instr

killL :: Instr -> LivenessDomain Temp
killL (OPER _ dsts srcs _) = LD (Set.fromList dsts)
killL (LABEL _ _         ) = LD Set.empty
killL (MOV _ d s         ) = LD (Set.singleton d)

genL :: Instr -> LivenessDomain Temp
genL (OPER _ dsts srcs _) = LD (Set.fromList srcs)
genL (LABEL _ _         ) = LD Set.empty
genL (MOV _ d s         ) = LD (Set.singleton s)

type LivenessAnalysis = StaticAnalysis LivenessDomain Temp Instr

newLP :: FCG Instr -> LivenessProgram
newLP fcg = P
  { ext    = length (labs fcg) - 1
  , tau    = LD Set.empty --vars fcg
  , kill   = killL
  , gen    = genL
  , flow   = G.pre (graph fcg)
    -- Los predecesores en el flujo inverso
  , flow_r = G.suc (graph fcg)
  , instr  = \n -> maybe (error "Nodo no encontrado") id (G.lab (graph fcg) n)
  , n      = length (labs fcg) - 1
  }

vars :: FCG Instr -> LivenessDomain Temp
vars fcg = LD $ getSetTemps (fst <$> (labs fcg))

getSetTemps :: [Instr] -> Set.Set Temp
getSetTemps = Set.unions . map getTemps

-- Seguro aca hay que sacar los especiales
getTemps :: Instr -> Set.Set Temp
getTemps (OPER _ dsts srcs _) =
  Set.union (Set.fromList dsts) (Set.fromList srcs)
getTemps (LABEL _ _) = Set.empty
getTemps (MOV _ d s) = Set.fromList [d, s]

newLA :: FCG Instr -> LivenessAnalysis
newLA fcg = SA {l = newLL $ runLD (vars fcg), p = newLP fcg}
