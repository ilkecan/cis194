module TestExercise6 where

import Exercise1 (fibs1)
import Exercise6
  ( fibs3,
    x,
  )
import Stream
  ( listToStream,
    streamToList,
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise6" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "x" $ x @?= listToStream [0, 1],
      testCase "x^4" $ (x ^ (4 :: Int)) @?= listToStream [0, 0, 0, 0, 1],
      testCase "(1 + x)^5" $
        ((1 + x) ^ (5 :: Int)) @?= listToStream [1, 5, 10, 10, 5, 1],
      testCase "(x^2 + x + 3) * (x - 5)" $
        ((x ^ (2 :: Int) + x + 3) * (x - 5)) @?= listToStream [-15, -2, -4, 1],
      testCase "x / x" $ x / x @?= listToStream [1],
      testCase "(x^3 - 4x) * (x - 2)" $
        (x * x * x - 4 * x) / (x - 2) @?= listToStream [0, 2, 1],
      testCase "take 30 fibs1 == (take 30 . streamToList) fibs3" $
        take 30 fibs1 @?= (take 30 . streamToList) fibs3
    ]
