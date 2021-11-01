module TestExercise6 where

import Exercise6
  ( HasVars (var),
    VarExprT (Add, Lit, Var),
    withVars,
  )
import Expr
  ( Expr (add, lit, mul),
  )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
  ( testCase,
    (@?=),
  )

unitTests :: [TestTree]
unitTests =
  [ testCase "" $ (add (lit 3) (var "x") :: VarExprT) @?= Add (Lit 3) (Var "x"),
    testCase "" $ withVars [("x", 6)] (add (lit 3) (var "x")) @?= Just 9,
    testCase "" $ withVars [("x", 6)] (add (lit 3) (var "y")) @?= Nothing,
    testCase "" $
      withVars [("x", 6), ("y", 3)] (mul (var "x") (add (var "y") (var "x")))
        @?= Just 54
  ]
