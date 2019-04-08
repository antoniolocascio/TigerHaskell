module TigerAssemTypes where

import           TigerSymbol
import           TigerTemp                     as Temp
import           Control.Monad.State
import           TigerTree                     as Tree
import           TigerUnique


type Assem = Symbol

data Instr =   OPER { assem :: MIPSAssem, dsts :: [Temp], srcs :: [Temp], jump :: Maybe [Label] }
             | LABEL { assem :: MIPSAssem, lab :: Label }
             | MOV { assem :: MIPSAssem, dst :: Temp, src :: Temp}
             | DIR { directive :: MIPSDir} deriving Show

data MIPSAssem =  J Label | Lbl Label | MOVE Temp Temp | SW Temp Int Temp
                | LI Temp Int | LA Temp Label | ADDI Temp Temp Int
                | ADD Temp Temp Temp | SUB Temp Temp Temp
                | MUL Temp Temp Temp | DIV Temp Temp Temp
                | AND Temp Temp Temp | ANDI Temp Temp Int
                | OR  Temp Temp Temp | ORI Temp Temp Int
                | XOR Temp Temp Temp | XORI Temp Temp Int
                | SLL Temp Temp Int | SRL Temp Temp Int | SRA Temp Temp Int
                | SLLV Temp Temp Temp | SRLV Temp Temp Temp | SRAV Temp Temp Temp
                | LW Temp Temp Int | BEQ Temp Temp Label | BNE Temp Temp Label
                | BLT Temp Temp Label | BLTU Temp Temp Label | BLE Temp Temp Label
                | BLEU Temp Temp Label | BGT Temp Temp Label | BGTU Temp Temp Label
                | BGE Temp Temp Label | BGEU Temp Temp Label | BLTZ Temp Label
                | BLEZ Temp Label | BGTZ Temp Label | BGEZ Temp Label
                | BEQZ Temp Label | BNEZ Temp Label | JAL Label | NOP | Empty
    deriving Show

data MIPSDir = DATA | GLOBL Label | ALIGN Int | SIZE Label Int | RDATA | FRAME Temp Int Temp | TEXT deriving Show


replaceTempAssem :: MIPSAssem -> Temp -> Temp -> MIPSAssem
replaceTempAssem (J   l       ) ot nt = J l
replaceTempAssem (Lbl l       ) ot nt = Lbl l
replaceTempAssem (MOVE t1 t2  ) ot nt = MOVE (rT t1 ot nt) (rT t2 ot nt)
replaceTempAssem (SW t1 i  t2 ) ot nt = SW (rT t1 ot nt) i (rT t2 ot nt)
replaceTempAssem (LW t1 t2 i  ) ot nt = LW (rT t1 ot nt) (rT t2 ot nt) i
replaceTempAssem (LI rd v     ) ot nt = LI (rT rd ot nt) v
replaceTempAssem (LA rd l     ) ot nt = LA (rT rd ot nt) l
replaceTempAssem (ADDI rd rs i) ot nt = ADDI (rT rd ot nt) (rT rs ot nt) i
replaceTempAssem (ADD rd rs rt) ot nt =
  ADD (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (SUB rd rs rt) ot nt =
  SUB (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (MUL rd rs rt) ot nt =
  MUL (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (DIV rd rs rt) ot nt =
  DIV (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (AND rd rs rt) ot nt =
  AND (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (ANDI rd rs i) ot nt = ANDI (rT rd ot nt) (rT rs ot nt) i
replaceTempAssem (OR rd rs rt) ot nt =
  OR (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (ORI rd rs i) ot nt = ORI (rT rd ot nt) (rT rs ot nt) i
replaceTempAssem (XOR rd rs rt) ot nt =
  XOR (rT rd ot nt) (rT rs ot nt) (rT rt ot nt)
replaceTempAssem (XORI rd rs i ) ot nt = XORI (rT rd ot nt) (rT rs ot nt) i
replaceTempAssem (SLL  rd rt sa) ot nt = SLL (rT rd ot nt) (rT rt ot nt) sa
replaceTempAssem (SRL  rd rt sa) ot nt = SRL (rT rd ot nt) (rT rt ot nt) sa
replaceTempAssem (SRA  rd rt sa) ot nt = SRA (rT rd ot nt) (rT rt ot nt) sa
replaceTempAssem (SLLV rd rt rs) ot nt =
  SLLV (rT rd ot nt) (rT rt ot nt) (rT rs ot nt)
replaceTempAssem (SRLV rd rt rs) ot nt =
  SRLV (rT rd ot nt) (rT rt ot nt) (rT rs ot nt)
replaceTempAssem (SRAV rd rt rs) ot nt =
  SRAV (rT rd ot nt) (rT rt ot nt) (rT rs ot nt)
replaceTempAssem (BEQ  rs rt l) ot nt = BEQ (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BNE  rs rt l) ot nt = BNE (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BLT  rs rt l) ot nt = BLT (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BLTU rs rt l) ot nt = BLTU (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BLE  rs rt l) ot nt = BLE (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BLEU rs rt l) ot nt = BLEU (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BGT  rs rt l) ot nt = BGT (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BGTU rs rt l) ot nt = BGTU (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BGE  rs rt l) ot nt = BGE (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BGEU rs rt l) ot nt = BGEU (rT rs ot nt) (rT rt ot nt) l
replaceTempAssem (BLTZ rs l   ) ot nt = BLTZ (rT rs ot nt) l
replaceTempAssem (BLEZ rs l   ) ot nt = BLEZ (rT rs ot nt) l
replaceTempAssem (BGTZ rs l   ) ot nt = BGTZ (rT rs ot nt) l
replaceTempAssem (BGEZ rs l   ) ot nt = BGEZ (rT rs ot nt) l
replaceTempAssem (BEQZ rs l   ) ot nt = BEQZ (rT rs ot nt) l
replaceTempAssem (BNEZ rs l   ) ot nt = BNEZ (rT rs ot nt) l
replaceTempAssem (JAL l       ) ot nt = JAL l
replaceTempAssem NOP            ot nt = NOP
replaceTempAssem Empty          ot nt = Empty

replaceTempInstr :: Instr -> Temp -> Temp -> Instr
replaceTempInstr (OPER as d s j) ot nt =
  OPER (replaceTempAssem as ot nt) (rT' ot nt <$> d) (rT' ot nt <$> s) j
replaceTempInstr (LABEL as l) ot nt = LABEL as l
replaceTempInstr (MOV as d s) ot nt =
  MOV (replaceTempAssem as ot nt) (rT d ot nt) (rT s ot nt)


rT :: Temp -> Temp -> Temp -> Temp
rT t ot nt | t == ot   = nt
           | otherwise = t

rT' ot nt t = rT t ot nt
