module TigerSres where

import           TigerTips
import           TigerTemp
import           TigerTrans
import           TigerFrame
import           TigerUnique
import           TigerExterna

--type FunEntry = (Unique, Label, [Tipo], Tipo, Externa)
type FunEntry = (Level, Label, [Tipo], Tipo, Externa)

type ValEntry = (Tipo, Access, Int)
  --Tipo -- Entrega2 -> = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
