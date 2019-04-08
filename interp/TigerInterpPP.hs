module TigerInterpPP
  ( renderCPU
  , renderStm
  , renderCode
  , renderWat
  , renderDat
  , renderMem
  , printCpu
  )
where

import           Prelude                 hiding ( (<>) )

import           TigerTemp
import           TigerTree
import           TigerInterpFrame

import           TigerSymbol

import           Data.Map                      as M
                                         hiding ( map )
import           TigerInterpTypes              as TI

import           Text.PrettyPrint
import           Data.List                     as L
import           TigerPrettyIr

prettyDato (TI.Str s) = text $ unpack s
prettyDato (DInt   i) = int i
prettyDato (FBody (accs, stms, _)) =
  hang (text "Function:") tabWidth
    $   text "Args:"
    <+> hsep (punctuate comma (prettyAccess <$> accs))
    $$  hang (text "Body:") tabWidth (vcat (map prettyStm stms))

prettyAccess :: Access -> Doc
prettyAccess = text . show

prettyMem :: M.Map Temp Int -> Doc
prettyMem m =
  vcat $ (\(t, i) -> text (unpack t) <+> text "->" <+> int i) <$> M.toList m

prettyWat :: M.Map Int Dato -> Doc
prettyWat m =
  vcat
    $   (\(i, d) -> int i <+> text "->" <+> prettyDato d)
    <$> (L.reverse $ M.toList m)

prettyDat :: M.Map Label Int -> Doc
prettyDat m =
  vcat $ (\(l, i) -> text (unpack l) <+> text "->" <+> int i) <$> M.toList m

prettyCPU :: CPU -> Doc
prettyCPU cpu =
  text "-------------------"
    $$ vcat
         [ hang (text "Mem:") tabWidth (prettyMem $ mem cpu)
         , hang (text "Wat:") tabWidth (prettyWat $ wat cpu)
         , hang (text "Dat:") tabWidth (prettyDat $ dat cpu)
         , text "Input:"
           <+> hsep (punctuate comma (text . unpack <$> input cpu))
         , text "Output:"
           <+> hsep (punctuate comma (text . unpack <$> output cpu))
         ]
    $$ text "-------------------"

renderMem :: M.Map Temp Int -> String
renderMem m = render (hang (text "Mem:") tabWidth (prettyMem m))

renderWat :: M.Map Int Dato -> String
renderWat m = render (hang (text "Wat:") tabWidth (prettyWat m))

renderDat :: M.Map Label Int -> String
renderDat m = render (hang (text "Dat:") tabWidth (prettyDat m))

renderCPU :: CPU -> String
renderCPU = render . prettyCPU

prettyCode :: [Stm] -> Doc
prettyCode stms =
  hang (text "Remaining code: ") tabWidth (vcat (map prettyStm stms))

renderCode :: [Stm] -> String
renderCode stms = render (prettyCode stms)

printCpu :: CPU -> String
printCpu cpu =
  "----------\n"
    ++ "RV : "
    ++ show (mem cpu ! rv)
    ++ "\n"
    ++ "Output:\n"
    ++ L.concat ((unstring . unpack) <$> output cpu)
    ++ "\n----------\n"
