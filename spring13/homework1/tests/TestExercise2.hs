module TestExercise2 where

import Exercise2 (doubleEveryOther)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise2" [qcProps, unitTests]

prop1 :: [Integer] -> Bool
prop1 numbers = (length . doubleEveryOther) numbers == length numbers

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . doubleEveryOther == length" prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "doubleEveryOther empty" $ doubleEveryOther [] @?= [],
      testCase "doubleEveryOther single" $ doubleEveryOther [3] @?= [3],
      testCase "doubleEveryOther even" $
        doubleEveryOther [8, 7, 6, 5] @?= [16, 7, 12, 5],
      testCase "doubleEveryOther odd" $
        doubleEveryOther [1, 2, 3] @?= [1, 4, 3]
    ]
