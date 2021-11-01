module TestExercise1 where

import Exercise1 (eval)
import ExprT (ExprT (Add, Lit, Mul))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise1" [qcProps, unitTests]

prop1 :: Integer -> Bool
prop1 n = eval (Lit n) == n

prop2 :: Integer -> Integer -> Bool
prop2 n1 n2 = eval (Add (Lit n1) (Lit n2)) == n1 + n2

prop3 :: Integer -> Integer -> Bool
prop3 n1 n2 = eval (Mul (Lit n1) (Lit n2)) == n1 * n2

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "eval (Lit n) == n" prop1,
      QC.testProperty "eval (Add (Lit n1) (Lit n2)) == n1 + n2" prop2,
      QC.testProperty "eval (Mul (Lit n1) (Lit n2)) == n1 * n2" prop3
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "eval" $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @?= 20
    ]
