{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TigerCanon
  ( linearize     -- | Stm -> [Stm]
  , basicBlocks   -- | [Stm] -> ([[Stm]] , Label)
  , traceSchedule -- | ( [[Stm]] , Label) -> [Stm]
  , firstTank
  , flattenCalls
  , Tank(..)
  )
where

import           TigerTemp
import           TigerTree
--import           State
import           TigerSymbol
import qualified TigerUnique                    ( StGen(..) )
import           Control.Monad.State
import           Data.Map.Strict               as M
import           TigerUnique
import           TigerFrame
import qualified Data.List                     as L

import           Prelude                 hiding ( lookup )
import           Debug.Trace

(%) :: Stm -> Stm -> Stm
(ExpS (Const _)) % x                = x
x                % (ExpS (Const _)) = x
x                % y                = Seq x y

inmut :: Exp -> Bool
inmut (Name  _    ) = True
inmut (Const _    ) = True
inmut (Temp  _    ) = True
inmut (Binop _ x y) = inmut x && inmut y
inmut _             = False

conmute :: Stm -> Exp -> Bool
conmute (ExpS (Const _)) _         = True
conmute _                (Name  _) = True
conmute _                (Const _) = True
conmute (ExpS (Call (Name xs) _)) _ | unpack xs == "_checkIndexArray" = True
                                    | unpack xs == "_checkNil"        = True
conmute (ExpS x) y = inmut x || inmut y
conmute _        _ = False

nop :: Stm
nop = ExpS (Const 0)

reorder :: (TLGenerator w, Monad w) => [Exp] -> w (Stm, [Exp])
reorder []                  = return (nop, [])
reorder (e@(Call _ _) : es) = do
  t <- newTemp
  reorder (Eseq (Move (Temp t) e) (Temp t) : es)
reorder (e : rest) = do
  (sts , e') <- doExp e
  (sts', el) <- reorder rest
  if conmute sts' e'
    then return (sts % sts', e' : el)
    else do
      t <- newTemp
      return (sts % Move (Temp t) e % sts', Temp t : el)

reorderExp :: (TLGenerator w, Monad w) => ([Exp], [Exp] -> Exp) -> w (Stm, Exp)
reorderExp (el, build) = do
  (sts, el') <- reorder el
  return (sts, build el')
reorderStm :: (TLGenerator w, Monad w) => ([Exp], [Exp] -> Stm) -> w Stm
reorderStm (el, build) = do
  (sts, el') <- reorder el
  return $ sts % build el'

doStm :: (TLGenerator w, Monad w) => Stm -> w Stm
doStm (Seq a b) = do
  a' <- doStm a
  b' <- doStm b
  return $ a' % b'
doStm (Jump e lb) = reorderStm ([e], \l -> Jump (prim l) lb)
doStm (CJump p a b t f) =
  reorderStm ([a, b], \l -> CJump p (prim l) (seg l) t f)
doStm (Move (Temp t) (Call e el)) =
  reorderStm (e : el, \l -> Move (Temp t) (Call (prim l) (tail l)))
doStm (Move (Temp t) b) = reorderStm ([b], Move (Temp t) . prim)
doStm (Move (Mem e) b) = reorderStm ([e, b], \l -> Move (Mem (prim l)) (seg l))
doStm (Move (Eseq s e) b) = doStm $ Seq s (Move e b)
doStm (ExpS (Call e el)) =
  reorderStm (e : el, \l -> ExpS $ Call (prim l) (tail l))
doStm (ExpS e) = reorderStm ([e], ExpS . head)
doStm s        = reorderStm ([], const s)

prim :: [a] -> a
prim = head

seg :: [a] -> a
seg = head . tail

doExp :: (TLGenerator w, Monad w) => Exp -> w (Stm, Exp)
doExp (Binop p a b) = reorderExp ([a, b], \l -> Binop p (prim l) (seg l))
doExp (Mem a      ) = reorderExp ([a], Mem . prim)
doExp (Eseq s e   ) = do
  sts        <- doStm s
  (sts', e') <- doExp e
  return (sts % sts', e')
doExp (Call e es) = reorderExp (e : es, \l -> Call (prim l) (tail l))
doExp e           = reorderExp ([], const e)

linear :: (Stm, [Stm]) -> [Stm]
linear (Seq a b, l) = linear (a, linear (b, l))
linear (s      , l) = s : l

linearize :: (TLGenerator w, Monad w) => Stm -> w [Stm]
linearize st = do
  s' <- doStm st
  return $ linear (s', [])

endblock
  :: (TLGenerator w, Monad w) => Label -> [[Stm]] -> [Stm] -> [Stm] -> w [[Stm]]
endblock done blist stms thisblock =
  blocks done stms (reverse thisblock : blist)

next
  :: (TLGenerator w, Monad w)
  => Label
  -> ([Stm] -> [Stm] -> w [[Stm]])
  -> [Stm]
  -> [Stm]
  -> w [[Stm]]
next _    eb (   l@Jump{}  : ls) rs = eb ls (l : rs)
next _    eb (   l@CJump{} : ls) rs = eb ls (l : rs)
next done eb ls@(Label l   : _ ) rs = next done eb (Jump (Name l) l : ls) rs
next done eb (   l         : ls) rs = next done eb ls (l : rs)
next done eb []                  rs = next done eb [Jump (Name done) done] rs

blocks :: (TLGenerator w, Monad w) => Label -> [Stm] -> [[Stm]] -> w [[Stm]]
blocks done (h@(Label _) : ls) bl = next done (endblock done bl) ls [h]
blocks _    []                 ls = return $ reverse ls
blocks done ls                 bl = do
  l <- newLabel
  blocks done (Label l : ls) bl

basicBlocks :: (TLGenerator w, Monad w) => [Stm] -> w ([[Stm]], Label)
basicBlocks stm = do
  done <- newLabel
  stm' <- blocks done stm []
  return (stm', done)

class Monad w => Trackable w where
    enterBlock' :: Label -> [Stm] -> w ()
    enterBlock :: [Stm] -> w ()
    enterBlock b@(Label s : _) = enterBlock' s b
    enterBlock _ = return ()
    getBlock :: Label -> w (Maybe [Stm])

data Obus = O {mapLS :: M.Map Label [Stm]}

firstTank :: Obus
firstTank = O {mapLS = M.empty}

type Tank = StateT Obus StGen

-- instance Environmental Tank where
--     data Mapper Tank a b = M (Map a b)
--     lookupI a (M m) = lookup a m
--     insertI k v (M m) = M $ insert k v m
--     intersecI f (M m1) (M m2) = M $intersectionWith f m1 m2
--     updateI k v (M m) = M $ insert k v m
--     emptyI = M empty

instance Trackable Tank where
    enterBlock' l b = do
        st <- get
        let nmap = insert l b (mapLS st)
        put st{mapLS=nmap}
    getBlock l = do
        st <- get
        return $ lookup l (mapLS st)

splitlast :: [a] -> ([a], a)
splitlast ls = (init ls, last ls)

traceR :: (Trackable w, TLGenerator w) => [Stm] -> [[Stm]] -> w [Stm]
traceR b@(Label lab : _) rs = do
  enterBlock' lab []
  case splitlast b of
    (most, Jump (Name nm) _) -> do
      gb <- getBlock nm
      case gb of
        Just b'@(_ : _) -> do
          t <- traceR b' rs
          return $ most ++ t
        _ -> do
          t <- getnext rs
          return $ b ++ t
    (most, CJump p x y t f) -> do
      gbt <- getBlock t
      gbf <- getBlock f
      case (gbt, gbf) of
        (_, Just b'@(_ : _)) -> do
          t <- traceR b' rs
          return $ b ++ t
        (Just b'@(_ : _), _) -> do
          te <- traceR b' rs
          return $ most ++ [CJump (notRel p) x y f t] ++ te
        _ -> do
          l'    <- newLabel
          rest' <- getnext rs
          return
            $  most
            ++ [CJump p x y t l', Label l', Jump (Name f) f]
            ++ rest'
    (_, Jump _ _) -> do
      t <- getnext rs
      return $ b ++ t
    _ -> error "Debería ser imposible"
traceR _ _ = error "Debería ser imposible"

getnext :: (Trackable w, TLGenerator w) => [[Stm]] -> w [Stm]
getnext (b@(Label l : _) : rs) = do
  gb <- getBlock l
  case gb of
    Just (_ : _) -> traceR b rs
    _            -> getnext rs
getnext [] = return []
getnext _  = error "No puede se'"

traceSchedule :: (Trackable w, TLGenerator w) => ([[Stm]], Label) -> w [Stm]
traceSchedule (blocks, done) = do
  mapM_ enterBlock blocks
  ls <- getnext blocks
  return $ ls ++ [Label done]


-- Creo que esto solo puede pasar con un nivel de profundidad (allocRecord, initArray)
flattenCalls :: (TLGenerator w, Monad w) => [Stm] -> w [Stm]
flattenCalls [] = return []
flattenCalls (ExpS (Call e args) : rest) =
  let indexedArgs = zip [0 ..] args
      argsCalls   = getCalls indexedArgs  --[(i, Call ..)]
  in  do
        tmps <- mapM (\_ -> newTemp) argsCalls
        let callMoves = L.concat $ zipWith
              (\(_, c) t -> [ExpS c, Move (Temp t) (Temp rv)])
              argsCalls
              tmps
            newArgs = replaceArgs
              indexedArgs
              (zipWith (\(i, _) t -> (i, t)) argsCalls tmps)
        flattenRest <- flattenCalls rest
        return $ callMoves ++ [ExpS (Call e newArgs)] ++ flattenRest
flattenCalls (s : rest) = do
  fr <- flattenCalls rest
  return $ s : fr

getCalls :: [(Int, Exp)] -> [(Int, Exp)]
getCalls []                      = []
getCalls ((i, Call e es) : rest) = (i, Call e es) : getCalls rest
getCalls (_              : rest) = getCalls rest

replaceArgs :: [(Int, Exp)] -> [(Int, Temp)] -> [Exp]
replaceArgs [] _  = []
replaceArgs c  [] = snd <$> c
replaceArgs ((i, e) : cod) ts@((j, t) : tmps)
  | i == j    = Temp t : replaceArgs cod tmps
  | otherwise = e : replaceArgs cod ts

canonM :: (Trackable w, TLGenerator w) => Stm -> w [Stm]
canonM st = do
  lin <- linearize st
  lss <- basicBlocks lin
  traceSchedule lss

-- canonEsp :: Stm -> TigerUnique.StGen [Stm]
-- canonEsp stm = do
--    lin <- linearize stm
--    lss <- basicBlocks lin
--    return (evalState (traceSchedule lss) firstTank)

-- canon :: Int -> Int -> [(Stm,a)] -> ([([Stm],a)], Int, Int)
-- canon tseed lseed frs = let
--     fsTank = firstTank {lgen = lseed, tgen = tseed}
--     (res,est) = runState (
--                     mapM (\(st,fr) -> do
--                             ss <- canonM st
--                             return (ss,fr)) frs) fsTank
--     in (res, tgen est, lgen est)
