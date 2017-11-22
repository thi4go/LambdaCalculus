module ChurchLibTests where

import Test.HUnit
import Lambda
import ChurchLib
import DeBruijnLib


-- Church Naturals tests
add1 = eval $ App (App add three) two
add2 = eval $ App (App add seven) one

sub1 = eval $ App (App sub seven) three
sub2 = eval $ App (App sub four) two

mult1 = eval $ App (App mult two) four
mult2 = eval $ App (App mult three) three

pred1 = eval $ App predecessor three
pred2 = eval $ App predecessor four

churchFac1 = eval $ App churchFac three
churchFac2 = eval $ App churchFac two

test9  = TestCase (assertEqual "church factorial - with three" (makeDebruijn six) (makeDebruijn churchFac1))
test10 = TestCase (assertEqual "church factorial - with two"   (makeDebruijn two) (makeDebruijn churchFac2))

test11 = TestCase (assertEqual "church addition - two + three" (makeDebruijn five)  (makeDebruijn add1))
test12 = TestCase (assertEqual "church addition - seven + one" (makeDebruijn eigth) (makeDebruijn add2))

test13 = TestCase (assertEqual "church subtraction - seven - three" (makeDebruijn four) (makeDebruijn sub1))
test14 = TestCase (assertEqual "church subtraction - four - two"    (makeDebruijn two)  (makeDebruijn sub2))

test16 = TestCase (assertEqual "church product - four x two"    (makeDebruijn eigth) (makeDebruijn mult1))
test15 = TestCase (assertEqual "church product - three x three" (makeDebruijn nine)  (makeDebruijn mult2))

test17 = TestCase (assertEqual "church predecessor - three" (makeDebruijn two)   (makeDebruijn pred1))
test18 = TestCase (assertEqual "church predecessor - four " (makeDebruijn three) (makeDebruijn pred2))

churchNatTests = TestList [ test9, test10, test11, test12, test13, test14,
                            test15, test16, test17, test18 ]


runChurchNatTests = runTestTT churchNatTests

-- Church Boolean tests
boolAnd1 = eval $ App (App churchAnd tre) fls
boolAnd2 = eval $ App (App churchAnd tre) tre
boolAnd3 = eval $ App (App churchAnd fls) fls

boolOr1  = eval $ App (App churchOr tre) fls
boolOr2  = eval $ App (App churchOr tre) tre
boolOr3  = eval $ App (App churchOr fls) fls

boolIsZero1 = eval $ App isZero zero
boolIsZero2 = eval $ App isZero one

test1 = TestCase (assertEqual "church - true && false  " fls (boolAnd1))
test2 = TestCase (assertEqual "church - true && true   " tre (boolAnd2))
test3 = TestCase (assertEqual "church - false && false " fls (boolAnd3))

test4 = TestCase (assertEqual "church - true || false  " tre (boolOr1))
test5 = TestCase (assertEqual "church - true || true   " tre (boolOr2))
test6 = TestCase (assertEqual "church - false || false " fls (boolOr3))

test7 = TestCase (assertEqual "is zero, zero?" tre (boolIsZero1))
test8 = TestCase (assertEqual "is one, zero? " fls (boolIsZero2))


churchBoolTests = TestList [ test1, test2, test3, test4, test5, test6 ]


runChurchBoolTests = runTestTT churchBoolTests
