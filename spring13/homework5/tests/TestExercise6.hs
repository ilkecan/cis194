module TestExercise6 where

import Exercise6
  ( HasVars (var),
    VarExprT (Add, Lit, Var),
    withVars,
  )
import Expr (Expr (add, lit, mul))
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise6" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "VarExprT" $
        (add (lit 3) (var "x") :: VarExprT) @?= Add (Lit 3) (Var "x"),
      testCase "with a variable" $
        withVars [("x", 6)] (add (lit 3) (var "x")) @?= Just 9,
      testCase "with an undefined variable" $
        withVars [("x", 6)] (add (lit 3) (var "y")) @?= Nothing,
      testCase "with two variables" $
        withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x")))
          @?= Just 54
    ]
