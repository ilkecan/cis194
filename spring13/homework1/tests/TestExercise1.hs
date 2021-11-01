module TestExercise1 where

import Data.Char (digitToInt)
import Exercise1 (toDigits)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise1" [qcProps, unitTests]

prop1 :: QC.Positive Integer -> Bool
prop1 (QC.Positive number) =
  toDigits number == map (toInteger . digitToInt) (show number)

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty
        "toDigits n == map (toInteger . digitToInt) (show n) where n > 0"
        prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "toDigits" $ toDigits 1234 @?= [1, 2, 3, 4],
      testCase "toDigits 0" $ toDigits 0 @?= [],
      testCase "toDigits negative" $ toDigits (-17) @?= []
    ]
