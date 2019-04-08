module TigerConsFoldIr where

----------------------------------------------------
-- Constant folding, Muchnick pág 337.
----------------------------------------------------

import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                )
import           TigerTree

cf :: Exp -> Exp
cf (Binop Plus (Const c1) (Const c2)) = Const (c1 + c2)
cf (Binop Plus (Binop Plus (Const c1) t) (Const c2)) =
  Binop Plus (Const (c1 + c2)) (cf t)
cf (Binop Plus t1 (Binop Plus t2 t3)) =
  Binop Plus (Binop Plus (cf t1) (cf t2)) (cf t3)
cf (Binop Plus  left       (Const c )) = Binop Plus (Const c) (cf left)
cf (Binop Minus (Const c1) (Const c2)) = Const (c1 - c2)
cf (Binop Minus left       (Const c )) = Binop Plus (Const (-c)) (cf left)
cf (Binop Mul   (Const c1) (Const c2)) = Const (c1 * c2)
cf (Binop Mul (Const c1) (Binop Plus (Const c2) t)) =
  Binop Plus (Const (c1 * c2)) (Binop Mul (Const c1) (cf t))
cf (Binop Mul (Binop Mul (Const c1) t) (Const c2)) =
  Binop Mul (Const (c1 * c2)) (cf t)
cf (Binop Mul (Binop Plus (Const c1) t) (Const c2)) =
  Binop Plus (Const (c1 * c2)) (Binop Mul (Const c2) (cf t))
cf (Binop Mul (Binop Plus t1 t2) (Const c)) =
  Binop Plus (Binop Mul (Const c) (cf t1)) (Binop Mul (Const c) (cf t2))
cf (Binop Mul t1 (Binop Mul t2 t3)) =
  Binop Mul (Binop Mul (cf t1) (cf t2)) (cf t3)
cf (Binop Mul (Const c) (Binop Plus t1 t2)) =
  Binop Plus (Binop Mul (Const c) (cf t1)) (Binop Mul (Const c) (cf t2))
cf (Binop Mul (Binop Minus t1 t2) (Const c)) =
  Binop Minus (Binop Mul (Const c) (cf t1)) (Binop Mul (Const c) (cf t2))
cf (Binop Mul (Const c) (Binop Minus t1 t2)) =
  Binop Minus (Binop Mul (Const c) (cf t1)) (Binop Mul (Const c) (cf t2))
cf (Binop Mul (Binop Plus t1 t2) t3) =
  Binop Plus (Binop Mul (cf t1) (cf t3)) (Binop Mul (cf t2) (cf t3))
cf (Binop Mul left (Const c)) = Binop Mul (Const c) (cf left)
cf (Binop Mul t1 (Binop Plus t2 t3)) =
  Binop Plus (Binop Mul (cf t1) (cf t3)) (Binop Mul (cf t2) (cf t3))
cf (Binop Mul (Binop Minus t1 t2) t3) =
  Binop Minus (Binop Mul (cf t1) (cf t3)) (Binop Mul (cf t2) (cf t3))
cf (Binop Mul t1 (Binop Minus t2 t3)) =
  Binop Minus (Binop Mul (cf t1) (cf t2)) (Binop Mul (cf t1) (cf t3))
cf (Binop oper left right) = Binop oper (cf left) (cf right)
-- Ahora el resto de las exp
cf (Mem e                ) = Mem (constFold e)
cf (Call f   explist     ) = Call f (constFold <$> explist)
cf (Eseq stm e           ) = Eseq (cfs stm) (constFold e)
cf e                       = e

cfs :: Stm -> Stm
cfs (Move exp1 exp2  ) = Move (constFold exp1) (constFold exp2)
cfs (ExpS e          ) = ExpS (constFold e)
cfs (Jump e labellist) = Jump (constFold e) labellist
cfs (CJump EQ (Const c1) (Const c2) l1 l2) =
  let l = if c1 == c2 then l1 else l2 in Jump (Name l) l
cfs (CJump NE (Const c1) (Const c2) l1 l2) =
  let l = if c1 /= c2 then l1 else l2 in Jump (Name l) l
cfs (CJump LT (Const c1) (Const c2) l1 l2) =
  let l = if c1 < c2 then l1 else l2 in Jump (Name l) l
cfs (CJump GT (Const c1) (Const c2) l1 l2) =
  let l = if c1 > c2 then l1 else l2 in Jump (Name l) l
cfs (CJump LE (Const c1) (Const c2) l1 l2) =
  let l = if c1 <= c2 then l1 else l2 in Jump (Name l) l
cfs (CJump GE (Const c1) (Const c2) l1 l2) =
  let l = if c1 >= c2 then l1 else l2 in Jump (Name l) l
cfs (CJump relop exp1 exp2 l1 l2) =
  CJump relop (constFold exp1) (constFold exp2) l1 l2
cfs (  Seq stm1 stm2) = Seq (cfs stm1) (cfs stm2)
cfs l@(Label _      ) = l

constFold :: Exp -> Exp
constFold e = let e' = cf e in if e /= e' then constFold e' else e'
constFoldS :: Stm -> Stm
constFoldS s = let s' = cfs s in if s /= s' then constFoldS s' else s'
