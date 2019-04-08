module TigerInterf where

import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Graph
import           TigerAssemTypes
import           TigerTemp
import           Data.Set                      as Set
import           Data.Maybe                    as M
import           Data.List                     as L
import           Data.Graph.Inductive.Basic
import           TigerFrame
import           Debug.Trace

makeIG :: [Temp] -> [Set.Set Temp] -> Gr Temp ()
makeIG nodes' t_sets
  = let
      nodes     = zip [0 ..] nodes'
      inv_nodes = (\(a, b) -> (b, a)) <$> nodes
      toKey t = M.fromMaybe (error $ "Temp no encontrado: " ++ show t)
                            (lookup t inv_nodes)
      edges = getEdges (Prelude.map (Set.map toKey) t_sets)
      gr    = undir $ mkGraph nodes edges
    in
      gr

getEdges :: [Set.Set Int] -> [UEdge]
getEdges [] = []
getEdges (s : rest) =
  let l_s       = Set.toList s
      new_edges = [ (x, y, ()) | x <- l_s, y <- l_s, x /= y ]
  in  L.union (getEdges rest) new_edges

makeIGsT :: [[Temp]] -> [[Set.Set Temp]] -> [Gr Temp ()]
makeIGsT nodesl l =
  let mregs  = Set.fromList machineregs
      nodes' = L.map (L.\\ machineregs) nodesl
      l'     = L.map (L.map (`difference` mregs)) l
  in  L.zipWith makeIG nodes' l'
