module DeBruijnTests where

import Test.HUnit

import DeBruijn
import DeBruijnLib


-- DeBruijn Naturals tests
add1 = eval $ BApp (BApp dbadd dbthree) dbtwo
add2 = eval $ BApp (BApp dbadd dbseven) dbone

sub1 = eval $ BApp (BApp dbsub dbseven) dbthree
sub2 = eval $ BApp (BApp dbsub dbfour) dbtwo

mult1 = eval $ BApp (BApp dbmult dbtwo) dbfour
mult2 = eval $ BApp (BApp dbmult dbthree) dbthree

pred1 = eval $ BApp dbpredecessor dbthree
pred2 = eval $ BApp dbpredecessor dbfour

debruijnFac1 = eval $ BApp dbFac dbthree
debruijnFac2 = eval $ BApp dbFac dbtwo

test9  = TestCase (assertEqual "debruijn factorial - with three" dbsix debruijnFac1)
test10 = TestCase (assertEqual "debruijn factorial - with two"   dbtwo debruijnFac2)

test11 = TestCase (assertEqual "debruijn addition - two + three" dbfive add1)
test12 = TestCase (assertEqual "debruijn addition - seven + one" dbeigth add2)

test13 = TestCase (assertEqual "debruijn subtraction - seven - three" dbfour sub1)
test14 = TestCase (assertEqual "debruijn subtraction - four - two"    dbtwo sub2)

test16 = TestCase (assertEqual "debruijn product - four x two"    dbeigth mult1)
test15 = TestCase (assertEqual "debruijn product - three x three" dbnine mult2)

test17 = TestCase (assertEqual "debruijn predecessor - three" dbtwo  pred1)
test18 = TestCase (assertEqual "debruijn predecessor - four " dbthree pred2)


debruijnNatTests = TestList [ test9, test10, test11, test12, test13, test14,
                            test15, test16, test17, test18 ]

rundebruijnNatTests = runTestTT debruijnNatTests

-- debruijn Boolean tests
boolAnd1 = eval $ BApp (BApp dbAnd dbtre) dbfls
boolAnd2 = eval $ BApp (BApp dbAnd dbtre) dbtre
boolAnd3 = eval $ BApp (BApp dbAnd dbfls) dbfls

boolOr1  = eval $ BApp (BApp dbOr dbtre) dbfls
boolOr2  = eval $ BApp (BApp dbOr dbtre) dbtre
boolOr3  = eval $ BApp (BApp dbOr dbfls) dbfls

boolIsZero1 = eval $ BApp dbisZero dbzero
boolIsZero2 = eval $ BApp dbisZero dbone

test1 = TestCase (assertEqual "debruijn - true && false  " dbfls (boolAnd1))
test2 = TestCase (assertEqual "debruijn - true && true   " dbtre (boolAnd2))
test3 = TestCase (assertEqual "debruijn - false && false " dbfls (boolAnd3))

test4 = TestCase (assertEqual "debruijn - true || false  " dbtre (boolOr1))
test5 = TestCase (assertEqual "debruijn - true || true   " dbtre (boolOr2))
test6 = TestCase (assertEqual "debruijn - false || false " dbfls (boolOr3))

test7 = TestCase (assertEqual "is zero, zero?" dbtre (boolIsZero1))
test8 = TestCase (assertEqual "is one, zero? " dbfls (boolIsZero2))


debruijnBoolTests = TestList [ test1, test2, test3, test4, test5, test6, test7, test8 ]


rundebruijnBoolTests = runTestTT debruijnBoolTests
