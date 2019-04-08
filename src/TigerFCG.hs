module TigerFCG where

import           Data.Graph.Inductive.Graph
import           TigerAssemTypes
import           Data.List                     as L
import           Data.Map                      as M
import           Data.Graph.Inductive.PatriciaTree
import           TigerTemp
import           Data.GraphViz                 as GV
import qualified Data.GraphViz.Attributes.Complete
                                               as GV
import qualified Data.GraphViz.Types           as GV
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TL
import qualified TigerSymbol                   as TS
import           Debug.Trace                    ( trace )
import           System.Directory
import qualified TigerTree                     as T
import           Data.Maybe                     ( fromMaybe )

data Flow = Next | Jump deriving (Eq, Show)

data FCG n = FCG {graph :: Gr n Flow, entry :: n, exit :: n, labs :: [(n, Int)], name :: Label}

class FCGNode n where
  getEdges :: [(n, Int)] -> [(n, Int)] -> [LEdge Flow]
  getName :: [n] -> Label

-- Primera instruccion es un label, la ultima un J RA getProcName (LABEL _ l : _) = l
makeFCG :: FCGNode n => [n] -> FCG n
makeFCG nodes =
  let lmap = zip nodes [0 ..]
      gr   = makeGraph lmap
  in  FCG
        { graph = gr
        , entry = head nodes
        , exit  = last nodes
        , labs  = lmap
        , name  = getName nodes
        }
 where

makeGraph :: FCGNode n => [(n, Int)] -> Gr n Flow
makeGraph lmap =
  let nodes = (\(a, b) -> (b, a)) <$> lmap
      edges = getEdges lmap lmap
  in  mkGraph nodes edges

  ----------------------------------------------------------------
  -- Instancias
  ----------------------------------------------------------------

instance FCGNode Instr where
  -- getEdges :: [(Instr, Int)] -> [(Instr, Int)] -> [LEdge Flow]
  getEdges []                 _ = []
  getEdges [n               ] _ = []
  getEdges ((inst, i) : rest) labels = case inst of
    OPER as _ _ (Just jmps) ->
      let jmps_ns   = lkup jmps labels
          new_edges = L.map (\d -> (i, d, Jump)) jmps_ns
      in  new_edges ++ getEdges rest labels
    _ -> (i, i + 1, Next) : getEdges rest labels
   where
    lkup :: [Label] -> [(Instr, Int)] -> [Int]
    lkup lbls lmap = L.map (lkup1 lmap) lbls
    lkup1 :: [(Instr, Int)] -> Label -> Int
    lkup1 [] l = error $ "Label: " ++ show l ++ " no esta"
    lkup1 ((LABEL _ l, i) : rest) lbl | l == lbl  = i
                                      | otherwise = lkup1 rest lbl
    lkup1 (_ : rest) lbl = lkup1 rest lbl
  getName (LABEL _ l : _) = l

-- Se asume canonizado, sin Eseq, seq
instance FCGNode T.Stm where
  getName (T.Label l : _) = l
  -- getEdges :: [(Stm, Int)] -> [(Stm, Int)] -> [LEdge Flow]
  getEdges [] _ = []
  getEdges [n] _ = []
  getEdges ((n, i) : rest) labels =
    let
     lkup l labels = fromMaybe (error "Label no se encuentra") (L.lookup (T.Label l) labels)
    in case n of
     T.Jump _ l -> (i, lkup l labels, Jump) : getEdges rest labels
     T.CJump _ _ _ l1 l2 -> (i, lkup l1 labels, Jump) : (i, lkup l2 labels, Jump) : getEdges rest labels
     _ -> (i, i + 1, Next) : getEdges rest labels


instance Labellable Instr where
  toLabelValue = GV.StrLabel . TL.pack . show

instance Labellable Flow where
  toLabelValue = GV.StrLabel . TL.pack . show

instance Labellable () where
  toLabelValue = GV.StrLabel . TL.pack . const ""
