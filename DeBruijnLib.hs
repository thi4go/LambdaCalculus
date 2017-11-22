module DeBruijnLib where

import Lambda hiding (eval, subst)
import DeBruijn

import ChurchLib

import Data.List


-- Conversão de um termo lambda para um termo DeBruijn.
-- Usa-se uma lista para controlar o contexto, chamada de env, para
-- obtermos o índice do lambda à qual aquela variável está ligada.
--
-- 1. Sempre que encontramos uma variável, procura-se o índice dela na
-- lista env, e renomeamos o termo.
-- 2. Sempre que encontramos uma abstração lambda, adicionamos a variável
-- correspondente à lista env
-- 3. A conversão da aplicação é direta

makeDebruijn :: Term -> BTerm
makeDebruijn = convert []
  where
    convert env (Var sym) = case (elemIndex sym env) of
      Just ind -> BVar ind
      Nothing  -> BVar (namingcontext sym)
    convert env (Lambda sym exp) = BLambda (convert (sym : env) exp)
    convert env (App e1 e2)      = BApp (convert env e1) (convert env e2)


-- Função que recebe dois termos lambda, e retorna True se forem equivalentes
alphaEquivalence :: Term -> Term -> Bool
alphaEquivalence = \x y -> makeDebruijn x == makeDebruijn y

-- As outras funções foram convertidas de forma direta, através da função
-- de conversão apresentada acima.
dbFac :: BTerm
dbFac = makeDebruijn churchFac

dbITE :: BTerm
dbITE = makeDebruijn churchITE

-- Booleans
dbtre :: BTerm
dbtre = makeDebruijn tre

dbfls :: BTerm
dbfls = makeDebruijn fls

dbisZero :: BTerm
dbisZero = makeDebruijn isZero

dbAnd :: BTerm
dbAnd = makeDebruijn churchAnd

dbOr :: BTerm
dbOr = makeDebruijn churchOr

-- Arithmetics
dbzero  = BLambda  $ BLambda $ (BVar 0)
dbone   = eval $ BApp dbsuccessor dbzero
dbtwo   = eval $ BApp dbsuccessor dbone
dbthree = eval $ BApp dbsuccessor dbtwo
dbfour  = eval $ BApp dbsuccessor dbthree
dbfive  = eval $ BApp dbsuccessor dbfour
dbsix   = eval $ BApp dbsuccessor dbfive
dbseven = eval $ BApp dbsuccessor dbsix
dbeigth = eval $ BApp dbsuccessor dbseven
dbnine  = eval $ BApp dbsuccessor dbeigth
dbten   = eval $ BApp dbsuccessor dbnine

dbadd :: BTerm
dbadd = makeDebruijn add

dbsub :: BTerm
dbsub = makeDebruijn sub

dbsuccessor :: BTerm
dbsuccessor = makeDebruijn successor

dbpredecessor :: BTerm
dbpredecessor = makeDebruijn predecessor

dbmult :: BTerm
dbmult = makeDebruijn mult
