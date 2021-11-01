module TestExercise6 where

import Exercise6 (hanoi4)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise6" [qcProps, unitTests]

-- from Table1 of
-- https://service.scs.carleton.ca/sites/default/files/tr/TR-04-10.pdf
hanoi4MinimumNumberOfMoves :: [Int]
hanoi4MinimumNumberOfMoves =
  [ 0,
    1,
    3,
    5,
    9,
    13,
    17,
    25,
    33,
    41,
    49,
    65,
    81,
    97,
    113,
    129,
    161,
    193,
    225,
    257,
    289
  ]

prop1 :: Integer -> Bool
prop1 n =
  length (hanoi4 n "a" "b" "c" "d")
    == hanoi4MinimumNumberOfMoves !! fromIntegral n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty
        "length . hanoi4 n == hanoi4MinimumNumberOfMoves !! n"
        $ forAll (chooseInteger (0, 20)) prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "hanoi4 0" $ hanoi4 0 "a" "b" "c" "d" @?= [],
      testCase "hanoi4 1" $ hanoi4 1 "a" "b" "c" "d" @?= [("a", "b")],
      testCase "hanoi4 2" $
        hanoi4 2 "a" "b" "c" "d" @?= [("a", "c"), ("a", "b"), ("c", "b")],
      testCase "length . hanoi4 15 == 129" $
        length (hanoi4 15 "a" "b" "c" "d") @?= 129
    ]
