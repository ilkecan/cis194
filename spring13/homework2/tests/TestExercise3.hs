module TestExercise3 where

import Exercise3 (build)
import Test.Tasty
import Test.Tasty.HUnit
import TestData
  ( sampleLogMessageTree,
    sampleLogMessages,
  )

test_all :: TestTree
test_all = testGroup "TestExercise3" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "build" $ build sampleLogMessages @?= sampleLogMessageTree
    ]
