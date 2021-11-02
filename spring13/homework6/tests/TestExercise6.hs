module TestExercise6 where

import Exercise6
  ( x,
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise6" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "show x" $
        show x @?= (show . take 20) (0 : 1 : repeat (0 :: Int))
    ]
