module ChurchLib where

import Lambda



-- Fatorial com church naturals e recursão natural, sem implementação nativa do Y/fix combinator
churchFac = Lambda "n" $
        App
            (App
                (App churchITE (App isZero (Var "n")))
                    (App successor zeroc))
                        (App (App mult (App churchFac (App predecessor (Var "n")))) (Var "n"))



-- Factorial com church naturals e tentativa de recursão com Y combinator / Fix point combinator
pureChurchFac = Lambda "f" $ Lambda "n" $
        App
            (App
                (App churchITE (App isZero (Var "n")))
                    (App successor zeroc))
                        (App (App mult (App (Var "f") (App predecessor (Var "n")))) (Var "n") )



-- Tentativa de implementação do combinator de recursão chamado Fix-point combinator,
-- de acordo com as orientações do livro. Obtive problemas em fazer funcionar
fix = Lambda "f" $ App (Lambda "x" $ App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))
                       (Lambda "x" $ App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))


-- Tentativa mais bruta de recursão através de um combinator do cálculo lambda, mas
-- também não obtive sucesso.
ycomb = Lambda "p" $ App (Lambda "f" $ App (Var "p") (App (Var "f") (Var "f"))) (Lambda "f" $ App (Var "p") (App (Var "f") (Var "f")))


-- A aplicação da função de fatorial com o termo fix ou ycomb evalua para o resultado errado.
-- BUG: além da evaluação errada, se eu passo o número cinco ou mais, resulta em um loop
--      infinito
facAppFix  = eval $ App (App fix pureChurchFac) three
facAppY    = eval $ App (App ycomb pureChurchFac) three


-- A função de fatorial com recursão nativa da linguagem Haskell funcionou sem problemas,
-- apenas para mostrar que podemos expressar a função fatorial puramente como um
-- termo Lambda.
facApp2    = eval $ App churchFac three


-- Implementação das funções por Lambda Termos


-- Conditions
churchITE :: Term
churchITE = Lambda "c" $ Lambda "t" $ Lambda "p" $ App (App (Var "c") (Var "t")) (Var "p")


-- Booleans
tre :: Term
tre  = Lambda "t" $ Lambda "f" (Var "t")

fls :: Term
fls = Lambda "t" $ Lambda "f" (Var "f")

isZero :: Term
isZero = Lambda "m" $ App (App (Var "m") (Lambda "x" fls)) tre

churchAnd :: Term
churchAnd = Lambda "v" $ Lambda "z" $ App (App (Var "v") (Var "z")) fls

churchOr :: Term
churchOr = Lambda "v" $ Lambda "z" $ App (App (Var "v") tre) (Var "z")



-- Arithmetics
zero  = Lambda "s" $ Lambda "z" (Var "z")
one   = eval $ App successor zero
two   = eval $ App successor one
three = eval $ App successor two
four  = eval $ App successor three
five  = eval $ App successor four
six   = eval $ App successor five
seven = eval $ App successor six
eigth = eval $ App successor seven
nine  = eval $ App successor eigth
ten   = eval $ App successor nine


successor :: Term
successor = Lambda "n" $ Lambda "s" $ Lambda "z" $ App (Var "s") (App (App (Var "n") (Var "s")) (Var "z"))

add :: Term
add = Lambda "m" $ Lambda "n" $ Lambda "u" $ Lambda "t" $ App (App (Var "m") (Var "u")) (App (App (Var "n") (Var "u")) (Var "t"))

sub :: Term
sub = Lambda "m" $ Lambda "n" $ App (App (Var "n") predecessor) (Var "m")

mult :: Term
mult = Lambda "m" $ Lambda "n" $ App (App (Var "n") (App add (Var "m"))) zeroc

zeroc = Lambda "s" $ Lambda "z" (Var "z")

predecessor :: Term
predecessor = Lambda "n" $ Lambda "f" $ Lambda "x" $ App (App (App (Var "n") (Lambda "g" $ Lambda "h" $ App (Var "h") (App (Var "g") (Var "f")))) (Lambda "u" (Var "x"))) (Lambda "u" (Var "u"))
