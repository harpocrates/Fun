module Fun.Internal (Var, Expr(..), Op(..), Type(..), TypingContext, eval, typeof) where

import Control.Monad (join, guard)

-- | Things where we can substitute in for a variable. This class enables us to
-- abstract subsitution over both types and terms.
class Substitutable a where
  -- | Capture avoiding substitution
  subst :: Var -> a -> a -> a

-- | Substitute only if a condition holds
substIf :: Substitutable a => Bool -> Var -> a -> a -> a
substIf b v e1 e2 = if b then subst v e1 e2 else e2

-- | Variables are represented by strings
type Var = String

-- | The terms in the language.
data Expr
  = Unit                            -- ^ Sole inhabitant of the type of the same name
  | Bool Bool                       -- ^ Boolean value
  | Num Int                         -- ^ Number value
  | Lam Var Type Expr               -- ^ Abstraction. The type describes the argument
  | App Expr Expr                   -- ^ Lambda application
  | Var Var                         -- ^ Variable
  | Binop Op Expr Expr              -- ^ Binary operation, one of +, -, <, =
  | If Expr Expr Expr               -- ^ Conditional
  | Rec Var Type Expr               -- ^ Recursive value
  | Inj1 Expr Type                  -- ^ Left introduction of sum  
  | Inj2 Type Expr                  -- ^ Right introduction of sum  
  | SumCase Expr Var Expr Var Expr  -- ^ Elimination of sum 
  | Prod Expr Expr                  -- ^ Introduction of product
  | ProdCase Expr Var Var Expr      -- ^ Elimination of product
  | Fold Expr Type                  -- ^ Introduction of fold (over iso-recursive types)
  | Unfold Expr                     -- ^ Elimination of fold (over iso-recursive types)

-- | The possible binary operations
data Op = Equals | LessThan | Plus | Minus

-- Term substitution
instance Substitutable Expr where
  subst v e (Lam v' ty e') | v /= v' = Lam v' ty (subst v e e')
  subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)
  subst v e (Var v') = if v == v' then e else Var v'
  subst v e (Binop o e1 e2) = Binop o (subst v e e1) (subst v e e2)
  subst v e (If b e1 e2) = If (subst v e b) (subst v e e1) (subst v e e2)
  subst v e (Rec v' ty e') | v /= v' = Rec v' ty (subst v e e')
  subst v e (Inj1 e' ty) = Inj1 (subst v e e') ty
  subst v e (Inj2 ty e') = Inj2 ty (subst v e e')
  subst v e (SumCase e1 v2 e2 v3 e3) = SumCase (subst v e e1) v2 (substIf (v2 /= v) v e e2) v3 (substIf (v3 /= v) v e e3)
  subst v e (Prod e1 e2) = Prod (subst v e e1) (subst v e e2)
  subst v e (ProdCase e1 v1 v2 e2) = ProdCase (subst v e e1) v1 v2 (if v1 /= v && v2 /= v then subst v e e2 else e2)
  subst v e (Fold e' ty) = Fold (subst v e e') ty
  subst v e (Unfold e') = Unfold (subst v e e')
  subst v e e' = e'

-- | Evaluation for expressions. This happens inside the 'Maybe' monad because
-- evaluations may fail on
--
--   * free variables
--   * 'App' applied to something that is not a 'Lam'
--   * 'If' branching on something that is not a 'Bool'
--   * 'SumCase' applied to something that is not a 'Inj1' or 'Inj2'
--   * 'ProdCase' applied to something that is not a 'Prod'
--   * 'Unfold' something that is not a 'Fold'
--
-- Recall that inside a 'Maybe' 'do'-block, failure to pattern match simply
-- returns with 'Nothing'.
eval :: Expr -> Maybe Expr
eval Unit = pure Unit 
eval (Bool b) = pure (Bool b)
eval (Num i) = pure (Num i)
eval (Lam x ty e) = pure (Lam x ty e)
eval (Var _) = fail "Free variable"
eval r@(Rec u _ e) = eval (subst u r e)
eval (Inj1 e ty) = Inj1 <$> eval e <*> pure ty
eval (Inj2 ty e) = Inj2 <$> pure ty <*> eval e
eval (Prod e1 e2) = Prod <$> eval e1 <*> eval e2
eval (Fold e ty) = Fold <$> eval e <*> pure ty
eval (App e1 e2) = do
  Lam v _ e <- eval e1 -- App expects its first argument to be a Lam
  e' <- eval e2
  eval (subst v e' e)
eval (Binop o e1 e2) = join (binop o <$> eval e1 <*> eval e2)
  where
    binop Equals   (Num n1) (Num n2) = pure (Bool (n1 == n2))
    binop LessThan (Num n1) (Num n2) = pure (Bool (n1 < n2))
    binop Plus     (Num n1) (Num n2) = pure (Num (n1 + n2))
    binop Minus    (Num n1) (Num n2) = pure (Num (n1 - n2))
    binop _        _        _        = fail "Invalid Binop"
eval (If e1 e2 e3) = do
  Bool b <- eval e1    -- If expects the scrutinee to be a boolean
  if b then eval e2 else eval e3
eval (SumCase e v1 e1 v2 e2) = do
  e' <- eval e
  case e' of           -- SumCase expects its scrutinee to be an Inj1 or Inj2
    Inj1 i1 _ -> eval (subst v1 i1 e1)
    Inj2 _ i2 -> eval (subst v2 i2 e2)
    _ -> fail "SumCase "
eval (ProdCase e v1 v2 e') = do
  Prod e1 e2 <- eval e -- ProdCase expects its scrutinee to be a Prod
  eval (subst v1 e1 (subst v2 e2 e'))
eval (Unfold e) = do
  Fold e' _ <- eval e  -- Unfold expects its argument to be a Fold
  pure e'


-- | The types in our language.
data Type
  = TNum           -- ^ Type of numbers
  | TBool          -- ^ Type of booleans
  | TUnit          -- ^ Unit type (1 in the semiring of types)
  | TVoid          -- ^ Void type (0 in the semiring of types)
  | Type :+ Type   -- ^ Sum type (addition in the semiring of types)
  | Type :* Type   -- ^ Product type (multiplication in the semiring of types)
  | Type :-> Type  -- ^ Function types
  | Mu Var Type    -- ^ Iso-recursive types
  | TVar Var       -- ^ Variables

-- Declare correct precedence and associativity for type connectives
infixr 4 :->
infixl 6 :+
infixl 8 :*

-- | This instance checks if two types are equal, where Mu types are equal up
-- to variable name.
instance Eq Type where
  -- Simple types are compared by identity
  TNum  == TNum  = True
  TBool == TBool = True
  TUnit == TUnit = True
  TVoid == TVoid = True
  -- Composite types are compared component-wise
  (t1 :+  t2) == (u1 :+  u2) = t1 == u1 && t2 == u2
  (t1 :*  t2) == (u1 :*  u2) = t1 == u1 && t2 == u2
  (t1 :-> t2) == (u1 :-> u2) = t1 == u1 && t2 == u2
  -- Mu types need alpha-equivalence
  (Mu v1 t1) == (Mu v2 t2) = t1 == subst v2 (TVar v1) t2
  (TVar v1) == (TVar v2) = v1 == v2
  -- Default case
  _ == _ = False

-- Type subsitution.
instance Substitutable Type where
  subst v ty (TVar v') = if v == v' then ty else TVar v'
  subst _ _ TNum = TNum
  subst _ _ TBool = TBool
  subst _ _ TUnit = TUnit
  subst _ _ TVoid = TVoid
  subst v ty (a :-> b) = subst v ty a :-> subst v ty b
  subst v ty (a :+ b) = subst v ty a :+ subst v ty b
  subst v ty (a :* b) = subst v ty a :* subst v ty b
  subst v ty (Mu v' ty') = if v == v' then Mu v' ty' else Mu v' (subst v ty ty')


type TypingContext = [(Var,Type)]

-- | Typing for expressions. This happens inside the 'Maybe' monad because
-- expressions may be ill-typed. In that case, 'Nothing' is returned.
typeof :: TypingContext -> Expr -> Maybe Type
typeof tc (Num n) = pure TNum
typeof tc (Bool b) = pure TBool
typeof tc Unit = pure TUnit
typeof tc (Rec v ty e) = typeof ((v,ty) : tc) e
typeof tc (Inj1 e1 ty2) = (:+) <$> typeof tc e1 <*> pure ty2
typeof tc (Inj2 ty1 e2) = (:+) <$> pure ty1 <*> typeof tc e2
typeof tc (Prod e1 e2) = (:*) <$> typeof tc e1 <*> typeof tc e2
typeof tc (Lam v a e) = (:->) <$> pure a <*> typeof ((v,a) : tc) e
typeof tc (Var v) = lookup v tc
typeof tc (Fold e mu) = do
    Mu v ty1 <- pure mu
    ty2 <- typeof tc e
    let ty2 = subst v (Mu v ty1) ty1
    guard (ty2 == ty2)
    pure mu
typeof tc (Unfold e) = do
    Mu v ty <- typeof tc e
    pure (subst v (Mu v ty) ty)
typeof tc (App e1 e2) = do
    a :-> b <- typeof tc e1
    a' <- typeof tc e2
    guard (a == a')
    pure b
typeof tc (Binop o e1 e2) = do
    TNum <- typeof tc e1
    TNum <- typeof tc e2
    case o of
        Equals -> pure TBool
        LessThan -> pure TBool
        Plus -> pure TNum
        Minus -> pure TNum
typeof tc (If e1 e2 e3) = do
    TBool <- typeof tc e1
    a1 <- typeof tc e2 
    a2 <- typeof tc e3
    guard (a1 == a2)
    pure a1
typeof tc (SumCase e v1 e1 v2 e2) = do
    a :+ b <- typeof tc e
    c1 <- typeof ((v1,a) : tc) e1
    c2 <- typeof ((v2,b) : tc) e2
    guard (c1 == c2)
    pure c1
typeof tc (ProdCase e v1 v2 e') = do
    a :* b <- typeof tc e
    typeof ((v1,a) : (v2,b) : tc) e'
