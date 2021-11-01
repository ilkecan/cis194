module TestExercise1 where

import Exercise1 (skips)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise1" [qcProps, unitTests]

prop1 :: [Int] -> Bool
prop1 list = (length . skips) list == length list

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . skips == length" prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "skips String 1" $ skips "ABCD" @?= ["ABCD", "BD", "C", "D"],
      testCase "skips String 2" $
        skips "hello!" @?= ["hello!", "el!", "l!", "l", "o", "!"],
      testCase "skips [Int]" $ skips [1 :: Integer] @?= [[1]],
      testCase "skips [Bool]" $
        skips [True, False] @?= [[True, False], [False]],
      testCase "skips []" $ skips ([] :: [Integer]) @?= []
    ]
