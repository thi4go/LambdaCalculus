module Lambda where

type Id = String

data Term = Var Id
          | Lambda Id Term
          | App Term Term
 deriving (Read, Eq)


instance Show Term where
 show (Var x) = x
 show (Lambda var term) = "\\" ++ var ++ " -> " ++ show term
 show (App term1 term2) = "(" ++ show term1 ++ ") (" ++ show term2 ++ ")"


-- Modifiquei a função eval para facilitar a visualização das aplicações dos
-- termos. Dessa forma, evaluamos o body de um termo lambda, e continuamos o eval
-- caso tenhamos uma aplicação dentro de outra aplicação.
eval :: Term -> Term
eval (Var v)             = Var v
eval (Lambda v body)     = Lambda v (eval body)
eval (App t1 t2)         = case (eval t1) of
     Lambda var body -> eval (subst var t2 body)
     t               -> App t (eval t2)


-- Substituição implementada em sala de aula
subst :: Id -> Term -> Term -> Term
subst v t (Var x)
 | v == x    = t
 | otherwise = Var x

subst v t1 (Lambda x t2)
 | v == x    = Lambda x t2
 | otherwise = Lambda x (subst v t1 t2)

subst v t1 (App t2 t3) = App (subst v t1 t2) (subst v t1 t3)
