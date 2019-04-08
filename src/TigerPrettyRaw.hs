module TigerPrettyRaw
  ( renderExpRaw
  )
where

import           Prelude                 hiding ( (<>) )
import           TigerAbs

import           Text.PrettyPrint
import           TigerSymbol

tabWidth :: Int
tabWidth = 4

prettyVar :: Var -> Doc
prettyVar (SimpleVar s) = text "SimpleVar" <+> text (unpack s)
prettyVar (FieldVar v s) =
  text "FieldVar" <+> parens (prettyVar v) <+> doubleQuotes (text $ unpack s)
prettyVar (SubscriptVar v e) =
  text "SubscriptVar" <+> parens (prettyVar v) <+> parens (prettyExp e)

prettyOp :: Oper -> Doc
prettyOp op = text $ show op

prettyTy :: Ty -> Doc
prettyTy (NameTy   s) = text "NameTy" <+> text (unpack s)
prettyTy (RecordTy r) = text "RecordTy" <+> brackets
  (hcat
    (punctuate
      (text ", ")
      (map
        (\(s, t) ->
          parens (doubleQuotes (text (unpack s)) <> comma <+> prettyTy t)
        )
        r
      )
    )
  )
prettyTy (ArrayTy s) = text "ArrayTy" <+> text (unpack s)

prettyField :: [(Symbol, Escapa, Ty)] -> Doc
prettyField fl = brackets $ hcat
  (punctuate
    (text ", ")
    (map
      (\(s, b, t) ->
        parens
          $   doubleQuotes (text (unpack s))
          <>  comma
          <+> text (show b)
          <>  comma
          <+> prettyTy t
      )
      fl
    )
  )

prettyDec :: Dec -> Doc
prettyDec (FunctionDec f) =
  text "FunctionDec"
    <+> lbrack
    $$  nest tabWidth (vcat $ map functionDec f)
    $$  rbrack
 where
  functionDec (s, f, Just r, e, _) =
    lparen
      <>  doubleQuotes (text $ unpack s)
      <>  comma
      <+> prettyField f
      <>  comma
      <+> (text "Just" <+> doubleQuotes (text $ unpack r))
      <>  comma
      $$  nest tabWidth (lparen <> prettyExp e $$ rparen)
      $$  rparen
  functionDec (s, f, Nothing, e, _) =
    lparen
      <>  doubleQuotes (text $ unpack s)
      <>  comma
      <+> prettyField f
      <>  comma
      <+> text "Nothing"
      $$  nest tabWidth (lparen <> prettyExp e $$ rparen)
      $$  rparen

prettyDec (VarDec s b (Just r) e _) =
  text "VarDec"
    <+> doubleQuotes (text $ unpack s)
    <+> text (show b)
    <+> (text "Just" <+> doubleQuotes (text $ unpack r))
    <+> parens (prettyExp e)
prettyDec (VarDec s b Nothing e _) =
  text "VarDec"
    <+> doubleQuotes (text $ unpack s)
    <+> text (show b)
    <+> text "Nothing"
    <+> parens (prettyExp e)
prettyDec (TypeDec f) = text "TypeDec" <+> brackets (vcat $ map typeDec f)
 where
  typeDec (s, ty, _) =
    parens $ doubleQuotes (text $ unpack s) <> comma <+> prettyTy ty

prettyExp :: Exp -> Doc
prettyExp (VarExp v _) = text "VarExp" <+> parens (prettyVar v)
prettyExp (UnitExp _ ) = text "UnitExp"
prettyExp (NilExp  _ ) = text "NilExp"
prettyExp (IntExp i _) = text "IntExp" <+> text (show i)
prettyExp (StringExp s _) =
  text "StringExp" <+> doubleQuotes (text (filter (/= '\n') s))
prettyExp (CallExp s args _) =
  text "CallExp" <+> doubleQuotes (text $ unpack s) <+> brackets
    (hcat (punctuate (text ", ") $ map (parens . prettyExp) args))
prettyExp (OpExp e1 op e2 _) =
  text "OpExp" <+> parens (prettyExp e1) <+> prettyOp op <+> parens
    (prettyExp e2)
prettyExp (RecordExp r n _) =
  text "RecordExp"
    <+> brackets
          (hcat
            (punctuate
              (text ", ")
              (map
                (\(s, e) -> doubleQuotes (text $ unpack s) <> comma <+> parens
                  (prettyExp e)
                )
                r
              )
            )
          )
    <+> doubleQuotes (text (unpack n))
prettyExp (SeqExp e _) =
  text "SeqExp"
    <> lbrack
    $$ nest tabWidth (vcat $ punctuate comma (map prettyExp e))
    $$ rbrack
prettyExp (AssignExp v e _) =
  text "AssignExp" <+> parens (prettyVar v) <+> parens (prettyExp e)
prettyExp (IfExp e e1 (Just e2) _) =
  text "IfExp"
    <+> parens (prettyExp e)
    <+> lparen
    $$  nest tabWidth (prettyExp e1)
    $$  rparen
    $$  text "Just"
    <+> lparen
    $$  nest tabWidth (prettyExp e2)
    $$  rparen
prettyExp (IfExp e e1 Nothing _) =
  text "IfExp"
    <+> parens (prettyExp e)
    <+> lparen
    $$  nest tabWidth (prettyExp e1)
    $$  rparen
    $$  text "Nothing"
prettyExp (WhileExp e e1 _) =
  text "WhileExp"
    <+> parens (prettyExp e)
    <+> lparen
    $$  nest tabWidth (prettyExp e1)
    $$  rparen
prettyExp (ForExp s b e1 e2 e3 _) =
  text "ForExp"
    <+> doubleQuotes (text $ unpack s)
    <+> text (show b)
    <+> parens (prettyExp e1)
    <+> parens (prettyExp e2)
    <+> lparen
    $$  nest tabWidth (prettyExp e3)
    $$  rparen
prettyExp (LetExp d e _) =
  text "LetExp "
    <+> lbrack
    $$  nest tabWidth (vcat $ punctuate comma (map prettyDec d))
    $$  rbrack
    $$  lparen
    $$  nest tabWidth (prettyExp e)
    $$  rparen
prettyExp (BreakExp _) = text "BreakExp"
prettyExp (ArrayExp s e1 e2 _) =
  text "ArrayExp"
    <+> doubleQuotes (text $ unpack s)
    <+> parens (prettyExp e1)
    <+> parens (prettyExp e2)

renderExpRaw :: Exp -> String
renderExpRaw = render . prettyExp
