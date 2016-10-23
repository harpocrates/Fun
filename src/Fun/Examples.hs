module Fun.Examples where

import Fun.Internal

listofNum :: Type
listofNum = Mu "list" (TUnit :+ (TNum :* TVar "list"))

nil :: Expr
nil = Fold (Inj1 Unit (TNum :* listofNum)) listofNum

cons :: Expr
cons = Lam "n" TNum (Lam "l" listofNum (Fold (Inj2 TUnit (Prod (Var "n") (Var "l"))) listofNum))

head :: Expr
head = Lam "list" listofNum (SumCase (Unfold (Var "list"))
                                     "empty" (Inj1 Unit TNum)
                                     "nonempty" (ProdCase (Var "nonempty") "head" "tail" (Inj2 TUnit (Var "x"))))
