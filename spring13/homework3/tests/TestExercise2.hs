module TestExercise2 where

import Exercise2 (localMaxima)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise2" [qcProps, unitTests]

prop1 :: [Integer] -> Bool
prop1 list = (length . localMaxima) list <= length list

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "length . localMaxima == length" prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "localMaxima multiple" $ localMaxima [2, 9, 5, 6, 1] @?= [9, 6],
      testCase "localMaxima single" $ localMaxima [2, 3, 4, 1, 5] @?= [4],
      testCase "localMaxima none" $ localMaxima [1, 2, 3, 4, 5] @?= [],
      testCase "localMaxima []" $ localMaxima [] @?= [],
      testCase "localMaxima [1]" $ localMaxima [1] @?= [],
      testCase "localMaxima [1,2]" $ localMaxima [1, 2] @?= []
    ]
