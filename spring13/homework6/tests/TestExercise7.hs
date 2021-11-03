module TestExercise7 where

import Exercise1 (fib)
import Exercise7
  ( Matrix (Matrix),
    fib4,
    fibonacciQMatrix,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise7" [qcProps, unitTests]

prop1 :: Integer -> Bool
prop1 n =
  fibonacciQMatrix ^ n == Matrix [[fib (n + 1), fib n], [fib n, fib (n - 1)]]

prop2 :: Integer -> Bool
prop2 n = fib4 n == fib n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "fibonacciQMatrix^n" $
        forAll (chooseInteger (1, 25)) prop1,
      QC.testProperty "fib4 n == fib n" $
        forAll (chooseInteger (0, 25)) prop2
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "matrix addition" $
        Matrix [[4, 3], [0, 5 :: Int]] + Matrix [[4, 2], [5, -2]]
          @?= Matrix [[8, 5], [5, 3]],
      testCase "abs matrix" $
        abs (Matrix [[-4, -3], [-0, 5 :: Int]]) @?= Matrix [[4, 3], [0, 5]],
      testCase "signum matrix" $
        signum (Matrix [[-4, 3], [-0, 5 :: Int]]) @?= Matrix [[-1, 1], [0, 1]],
      testCase "7 :: Matrix Int" $
        (7 :: Matrix Int) @?= Matrix [[7, 0], [0, 7]],
      testCase "negate matrix" $
        negate (Matrix [[-4, 3], [-0, 5 :: Int]]) @?= Matrix [[4, -3], [0, -5]]
    ]
