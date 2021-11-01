module TestExercise2 where

import Exercise2 (evalStr)
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise2" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "evalStr valid 1" $ evalStr "2" @?= Just 2,
      testCase "evalStr valid 2" $ evalStr "(2+3)*4" @?= Just 20,
      testCase "evalStr valid 3" $ evalStr "2+3*4" @?= Just 14,
      testCase "evalStr invalid" $ evalStr "2+3*" @?= Nothing
    ]
