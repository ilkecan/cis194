module TestExercise5 where

import Exercise5
  ( nats,
    ruler,
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise5" [unitTests]

rulerSequence :: [Int]
rulerSequence = [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "show nats" $ show nats @?= show [0 .. 19 :: Int],
      testCase "show ruler" $ show ruler @?= show rulerSequence
    ]
