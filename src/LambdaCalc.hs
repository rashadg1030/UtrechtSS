module LambdaCalc where

data Term = App Term Term
          | Var String
          | Lam String Term

eval :: Term -> Term
eval (App t1 t2) = case t1 of
    Lam arg term -> case t2 of
        Var name       -> term
        Lam arg' term' -> Lam arg t1
        App t1' t2'    -> case term of
            Var name' -> if arg == name' then
            _         -> undefined
    _            -> error "Can't apply nonLambbdas"
eval (Lam arg term) = Lam arg $ eval term
eval (Var name) = Var name

subst :: Term -> String -> Term
subst (Var name) = undefined
subst (Lam param term) arg = case term of
    Var name -> case param == name of
        True  -> arg
        False -> Var name

term1 = App (Lam "x" (Var "x")) (Var "b") -> (Var "b")

term2 = App (Lam "x" (Var "y")) (Var "b") -> (Var "y")


