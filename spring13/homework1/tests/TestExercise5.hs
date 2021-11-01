module TestExercise5 where

import Exercise5 (hanoi)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise5" [qcProps, unitTests]

prop1 :: Integer -> Bool
prop1 n = length (hanoi n "a" "b" "c") == 2 ^ n - 1

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . hanoi n == 2^n - 1" $
        forAll (chooseInteger (0, 10)) prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "hanoi 0" $ hanoi 0 "a" "b" "c" @?= [],
      testCase "hanoi 1" $ hanoi 1 "a" "b" "c" @?= [("a", "b")],
      testCase "hanoi 2" $
        hanoi 2 "a" "b" "c" @?= [("a", "c"), ("a", "b"), ("c", "b")]
    ]
