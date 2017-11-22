module Compiler where

import ChurchLib
import RBL as R
import Lambda as L


-- A tradução foi feita de maneira bem direta, aplicando os termos ja
-- implementadas na ChurchLib equivalentes as expressões do RBL-Calculus
-- passadas à função.

translate :: R.Expression -> L.Term
-- Booleans
translate (B True)         = tre
translate (B False)        = fls
translate (And lexp rexp)  = App (App churchAnd (translate lexp)) (translate rexp)
translate (Or lexp rexp)   = App (App churchOr (translate lexp)) (translate rexp)

-- Conditions
translate (IfThenElse c exp1 exp2) = App (App (App churchITE (translate c)) (translate exp1)) (translate exp2)

-- Arithmetics
translate (N n)           = convertNat (N n)
translate (Add lexp rexp) = App (App add (translate lexp)) (translate rexp)
translate (Sub lexp rexp) = App (App sub (translate lexp)) (translate rexp)

-- Lambda
translate (Ref id)         = Var id
translate (Abs id exp)     = Lambda id (translate exp)
translate (Appl exp1 exp2) = App (translate exp1) (translate exp2)


-- Helpers
convertNat :: R.Expression -> L.Term
convertNat (N 0) = Lambda "s" $ Lambda "z" (Var "z")
convertNat   n   = L.eval $ App successor (convertNat (R.eval m))
    where m = R.Sub n (N 1)





-- TESTS
-- L.eval $ translate (IfThenElse (B True) (And (B True) (B False)) (Or (B True) (B False)))
-- L.eval $ translate (Or (B False) (B False))
-- L.eval $ translate (And (B False) (B True))
