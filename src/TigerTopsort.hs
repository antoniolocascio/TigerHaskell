module TigerTopsort
  ( topsort
  )
where

-- Implementacion del algoritmo de Kahn de topsort
import           Data.List

topsort :: Eq v => [(v, v)] -> Maybe [v]
topsort g =
  let
    -- s: Nodes with no incoming edge. Tipos ya definidos, fuera del batch.
      s = (fst <$> g) \\ (snd <$> g)
  in  do
        l <- topsort_ g [] s
        return (l \\ s)

topsort_ :: Eq v => [(v, v)] -> [v] -> [v] -> Maybe [v]
topsort_ []      l [] = Just l
topsort_ (_ : _) _ [] = Nothing
topsort_ g l (n : s) =
  let l' = l ++ [n]
      es = filter (\(a, b) -> a == n) g
      g' = g \\ es
      ms = filter (noIncomingEdge g') (snd <$> es)
      s' = s ++ ms
  in  topsort_ g' l' s'
  where noIncomingEdge g m = all (\(a, b) -> b /= m) g
