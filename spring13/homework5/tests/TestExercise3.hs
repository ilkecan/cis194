module TestExercise3 where

import Expr (Expr (add, lit, mul))
import ExprT (ExprT (Add, Lit, Mul))
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise3" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "Expr :: ExprT == ExprT" $
        (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
          @?= Mul (Add (Lit 2) (Lit 3)) (Lit 4)
    ]
