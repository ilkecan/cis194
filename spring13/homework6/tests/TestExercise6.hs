module TestExercise6 where

import Exercise6
  ( x,
  )
import Test.Tasty
import Test.Tasty.HUnit

test_all :: TestTree
test_all = testGroup "TestExercise6" [unitTests]

streamSeq1 :: [Int]
streamSeq1 = [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

streamSeq2 :: [Int]
streamSeq2 = [1, 5, 10, 10, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

streamSeq3 :: [Int]
streamSeq3 = [-15, -2, -4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "show x" $
        show x @?= (show . take 20) (0 : 1 : repeat (0 :: Int)),
      testCase "x^4" $ show (x ^ (4 :: Int)) @?= show streamSeq1,
      testCase "(1 + x)^5" $ show ((1 + x) ^ (5 :: Int)) @?= show streamSeq2,
      testCase "(x^2 + x + 3) * (x - 5)" $
        show ((x ^ (2 :: Int) + x + 3) * (x - 5)) @?= show streamSeq3
    ]
