module TestExercise4 where

import Exercise4 (validate)
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise4" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "validate true" $ validate 4012888888881881 @?= True,
      testCase "validate false" $ validate 4012888888881882 @?= False
    ]
