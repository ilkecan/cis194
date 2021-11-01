module TestExercise4 where

import Exercise4 (sieveSundaram)
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise4" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "sieveSundaram 2" $ sieveSundaram 2 @?= [3, 5],
      testCase "sieveSundaram 0" $ sieveSundaram 0 @?= [],
      testCase "sieveSundaram -2" $ sieveSundaram (-2) @?= [],
      testCase "length . sieveSundaram $ 5000" $
        (length . sieveSundaram) 5000 @?= 1228
    ]
