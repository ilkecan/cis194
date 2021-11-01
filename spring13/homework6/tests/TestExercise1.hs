module TestExercise1 where

import Exercise1 (fib)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise1" [qcProps, unitTests]

fibonacciSequence :: [Integer]
fibonacciSequence =
  [ 0,
    1,
    1,
    2,
    3,
    5,
    8,
    13,
    21,
    34,
    55,
    89,
    144,
    233,
    377,
    610,
    987,
    1597,
    2584,
    4181,
    6765
  ]

prop1 :: Int -> Bool
prop1 n = (fib . toInteger) n == fibonacciSequence !! n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "fib n = fibonacciSequence !! n" $
        forAll (chooseInt (0, 20)) prop1
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "fib zero" $ fib 0 @?= 0,
      testCase "fib one" $ fib 1 @?= 1,
      testCase "fib negative even" $ fib (-6) @?= -8,
      testCase "fib negative odd" $ fib (-5) @?= 5,
      testCase "fib ten" $ fib 10 @?= 55
    ]
