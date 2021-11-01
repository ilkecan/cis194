module TestExercise4 where

import Expr
  ( Expr (add, lit, mul),
    MinMax (MinMax),
    Mod7 (Mod7),
  )
import Parser (parseExp)
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise4" [unitTests]

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "testExp :: Maybe Integer" $
        (testExp :: Maybe Integer) @?= Just (-7),
      testCase "testExp :: Maybe Bool" $
        (testExp :: Maybe Bool) @?= Just True,
      testCase "testExp :: Maybe MinMax" $
        (testExp :: Maybe MinMax) @?= Just (MinMax 5),
      testCase "testExp :: Maybe Mod7" $
        (testExp :: Maybe Mod7) @?= Just (Mod7 0)
    ]
