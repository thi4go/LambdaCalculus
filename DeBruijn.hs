module DeBruijn where

import Lambda hiding (eval, subst)
import ChurchLib

import Data.Maybe
import Data.List


type Sym = Int

-- Termos De Bruijn , ou Nameless Lambda Terms
data BTerm = BVar Sym
           | BLambda BTerm
           | BApp BTerm BTerm
 deriving (Read, Eq)


instance Show BTerm where
  show (BVar x)           = show x
  show (BLambda term)     = "\\ -> " ++ show term
  show (BApp term1 term2) = "(" ++ show term1 ++ ") (" ++ show term2 ++ ")"



-- Função para renumerar os índices das variáveis livres dos termos.
-- Quando vamos substituir, o contexto onde ela ocorre fica um índice
-- mais longo. Portando, é necessário shiftar esse índice, para que as
-- variáveis livres continuem referenciando aos mesmos nomes no novo contexto
-- criado pela substituição

type Cutoff = Int
type DShift = Int

shifting :: DShift -> Cutoff -> BTerm -> BTerm
shifting d c (BVar k)
  | k < c  = BVar k
  | k >= c = BVar (k+d)
shifting d c (BLambda t)  = BLambda (shifting d (c+1)  t)
shifting d c (BApp t1 t2) = BApp (shifting d c t1) (shifting d c t2)



-- A evaluação dos termos DeBruijn é feita de forma análoga, com pequenas
-- mudanças, e a aplicação do shift (segundo as orientações do livro) nomes
-- casos necessários

eval :: BTerm -> BTerm
eval (BVar v)            = BVar v
eval (BLambda body)      = BLambda (eval body)
eval (BApp t1 t2)        = case (eval t1) of
     BLambda body   -> eval (shifting (-1) 0 (subst 0 (shifting 1 0 t2) body))
     t              -> BApp t (eval t2)




-- A substituição também é feita de forma análoga, mas shiftando os índices
-- no caso de uma abstração lambda, de acordo com as orientações do livro

subst :: Sym -> BTerm -> BTerm -> BTerm
subst j s (BVar x)
  | j == x    = s
  | otherwise = BVar x
subst j s (BLambda t)  = BLambda (subst (j+1) (shifting 1 0 s) t)
subst j s (BApp t1 t2) = BApp (subst j s t1) (subst j s t2)



-- Contexto usado para nomear variáveis livres
namingcontext :: [Char] -> Int
namingcontext c = case c of
   "x" -> 0
   "y" -> 1
   "s" -> 2
   "z" -> 3
   "a" -> 4
   "b" -> 5
   "t" -> 6
   "u" -> 7
   "m" -> 8
   "n" -> 9
