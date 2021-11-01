module TestExercise2 where

import Exercise1 (fibs1)
import Exercise2 (fibs2)
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise1" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "take 30 fibs1 == take 30 fibs2" $
        take 30 fibs1 @?= take 30 fibs2
    ]
