{-# LANGUAGE OverloadedStrings #-}

module Fun.Pretty where

import Fun.Internal

import Text.PrettyPrint.HughesPJ

-- | Things that can be pretty printed.
class Pretty a where
  {-# MINIMAL prettyP | pretty #-}
  -- | Pretty print something in a context of a given precedence
  prettyP :: Int -> a -> Doc
  prettyP _ = pretty
  -- | Pretty print something in a minimal context
  pretty :: a -> Doc
  pretty = prettyP 0

-- Pretty instance for terms is a mix of syntax that looks like Haskell/ML
instance Pretty Expr where
  prettyP _ Unit = "unit"
  prettyP _ (Bool True) = "True"
  prettyP _ (Bool False) = "False"
  prettyP _ (Num n) = int n
  prettyP d (App e1 e2) = maybeParens (d > 8) (hang (prettyP 8 e1) 2 (prettyP 9 e2))
  prettyP _ (Var v) = text v
  prettyP d (Binop Equals e1 e2) = maybeParens (d > 3) (prettyP 4 e1 <+> "=" <+> prettyP 4 e2)
  prettyP d (Binop LessThan e1 e2) = maybeParens (d > 3) (prettyP 4 e1 <+> "<" <+> prettyP 4 e2)
  prettyP d (Binop Plus e1 e2) = maybeParens (d > 4) (prettyP 4 e1 <+> "+" <+> prettyP 5 e2)
  prettyP d (Binop Minus e1 e2) = maybeParens (d > 4) (prettyP 4 e1 <+> "-" <+> prettyP 5 e2)
  prettyP d (Inj1 e1 ty) = maybeParens (d > 5) ("inj1" <+> prettyP 6 e1 <+> prettyP 10 ty)
  prettyP d (Inj2 ty e2) = maybeParens (d > 5) ("inj2" <+> prettyP 10 ty <+> prettyP 6 e2)
  prettyP d (Prod e1 e2) = maybeParens (d > 5) ("pair" <+> prettyP 6 e1 <+> prettyP 6 e2)
  prettyP d (Fold e ty) = 
    maybeParens (d > 5) (hang ("fold" <+> prettyP 6 e) 2
                              ("into" <+> prettyP 10 ty))
  prettyP d (Unfold e) = maybeParens (d > 5) ("unfold" <+> prettyP 6 e)
  prettyP d (Lam v ty e) =
    maybeParens (d > 3) (hang ("fun" <+> text v <> ":" <+> prettyP 0 ty <+> "->") 2
                              (prettyP 3 e))
  prettyP d (Rec v ty e) =
    maybeParens (d > 3) (hang ("rec" <+> text v <> ":" <+> prettyP 0 ty <+> "=") 2
                              (prettyP 3 e))
  prettyP d (If b e2 e3) =
    maybeParens (d > 8) (hang ("if" <+> prettyP 0 b) 2
                              (sep ["then" <+> prettyP 0 e2
                                   ,"else" <+> prettyP 0 e2]))
  prettyP d (ProdCase e v u eB) =
    maybeParens (d > 5) (hang ("prod-case" <+> prettyP 0 e <+> "of") 2
                              ("(" <> text v <> "," <+> text u <> ")" <+> "->" <+> prettyP 0 eB))
  prettyP d (SumCase e v e1 u e2) =
    maybeParens (d > 5) (hang ("sum-case" <+> prettyP 0 e <+> "of") 2
                              (sep ["inj1" <+> text v <+> "->" <+> prettyP 0 e1 <> ";"
                                   ,"inj2" <+> text u <+> "->" <+> prettyP 0 e2]))

-- Pretty instance for types is a mix of syntax that looks like Haskell/ML
instance Pretty Type where
  prettyP _ TNum  = "num"
  prettyP _ TBool = "bool"
  prettyP _ TUnit = "1"
  prettyP _ TVoid = "0"
  prettyP d (ty1 :+  ty2) = maybeParens (d > 6) (prettyP 6 ty1 <+> "+"  <+> prettyP 7 ty2)
  prettyP d (ty1 :*  ty2) = maybeParens (d > 8) (prettyP 8 ty1 <+> "*"  <+> prettyP 9 ty2)
  prettyP d (ty1 :-> ty2) = maybeParens (d > 4) (prettyP 5 ty1 <+> "->" <+> prettyP 4 ty2)
  prettyP d (Mu v ty)     = maybeParens (d > 2) ("mu" <+> text v <> "." <+> prettyP 2 ty)
  prettyP _ (TVar v) = text v

-- Use the pretty instance for showing types and terms
instance Show Expr where { show = show . pretty }
instance Show Type where { show = show . pretty }