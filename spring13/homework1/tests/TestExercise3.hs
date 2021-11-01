module TestExercise3 where

import Exercise3 (sumDigits)
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise3" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "sumDigits" $ sumDigits [16, 7, 12, 5] @?= 22,
      testCase "sumDigits empty" $ sumDigits [] @?= 0,
      testCase "sumDigits zero" $ sumDigits [0] @?= 0
    ]
