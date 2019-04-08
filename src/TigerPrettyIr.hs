module TigerPrettyIr where


import           Prelude                 hiding ( (<>) )

import           TigerTemp
import           TigerTree

import           TigerSymbol

import           TigerFrame
import           TigerTrans
import           Data.Map                      as M
                                         hiding ( map )
import           TigerInterpTypes        hiding ( (!!) )

import           Text.PrettyPrint
import           Data.List

tabWidth = 2

prettyRelop :: Relop -> Doc
prettyRelop = text . show

prettyBop :: BOp -> Doc
prettyBop = text . show

prettyExp :: Exp -> Doc
prettyExp (Const i) = int i
prettyExp (Name  l) = text $ makeStringL l
prettyExp (Temp  l) = text $ makeStringT l
prettyExp (Binop b l r) =
  text "BOP"
    <>  prettyBop b
    <+> (parens $ prettyExp l)
    <+> (parens $ prettyExp r)
prettyExp (Mem e) = text "M" <> (brackets $ prettyExp e)
prettyExp (Call e args) =
  prettyExp e <> (parens $ cat $ punctuate semi (map prettyExp args))
prettyExp (Eseq s e) = prettyStm s <> semi $+$ prettyExp e

prettyStm :: Stm -> Doc
prettyStm (Move e1 e2) = prettyExp e1 <> text "<-" <> prettyExp e2
prettyStm (ExpS e    ) = text "E" <+> prettyExp e
prettyStm (Jump e _  ) = text "J" <+> parens (prettyExp e)
prettyStm (CJump rel e1 e2 l1 l2) =
  text "CJ"
    <>  prettyRelop rel
    <+> (parens $ prettyExp e1)
    <+> (parens $ prettyExp e2)
    <+> (text $ makeStringL l1)
    <+> (text $ makeStringL l2)
prettyStm (Seq l r) = prettyStm l <> semi $+$ prettyStm r
prettyStm (Label l) = text "Lab:" <> (text $ makeStringL l)

prettyFrame :: Frame -> Doc
prettyFrame (Frame nm fs ls aArg aLoc aReg mArgs) = brackets (text $ unpack nm)

prettyFrag :: Frag -> Doc
prettyFrag (Proc    s f ) = (prettyFrame f <> text ":") $+$ prettyStm s
prettyFrag (AString l ts) = ((text "Str") <+> (text $ makeStringL l))
  $+$ cat (map (\t -> text "\t" <> (text $ unpack t)) ts)

prettyPCan :: [Stm] -> Frame -> Doc
prettyPCan st fr =
  (prettyFrame fr <> text ":") $+$ nest tabWidth (vcat (map prettyStm st))

prettyBExp :: BExp -> Doc
prettyBExp (Ex e) = text "Ex" <+> prettyExp e
prettyBExp (Nx s) = text "Nx" <+> prettyStm s
prettyBExp _      = text "WAT!"

renderStm :: Stm -> String
renderStm = render . prettyStm
renderFrag :: Frag -> String
renderFrag = render . prettyFrag
renderPCan :: [Stm] -> Frame -> String
renderPCan a b = render $ prettyPCan a b
renderIr :: Exp -> String
renderIr = render . prettyExp
renderBIr :: BExp -> String
renderBIr = render . prettyBExp

renderCanon :: ([Frag], [([Stm], Frame)]) -> String
renderCanon (strs, procs) =
  intercalate "\n" (map renderFrag strs) ++ "\n" ++ intercalate
    "\n"
    (map (uncurry renderPCan) procs)
