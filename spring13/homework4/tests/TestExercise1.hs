module TestExercise1 where

import Exercise1
  ( fun1,
    fun2,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

test_all :: TestTree
test_all = testGroup "TestExercise1" [qcProps, unitTests]

fun1Baseline :: [Integer] -> Integer
fun1Baseline [] = 1
fun1Baseline (x : xs)
  | even x = (x - 2) * fun1Baseline xs
  | otherwise = fun1Baseline xs

prop1 :: [Integer] -> Bool
prop1 ns = fun1 ns == fun1Baseline ns

fun2Baseline :: Integer -> Integer
fun2Baseline 1 = 0
fun2Baseline n
  | even n = n + fun2Baseline (n `div` 2)
  | otherwise = fun2Baseline (3 * n + 1)

prop2 :: QC.Positive Integer -> Bool
prop2 (QC.Positive n) = fun2 n == fun2Baseline n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "fun1 == fun1Baseline " prop1,
      QC.testProperty "fun2 == fun2Baseline" prop2
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "fun1 []" $ fun1 [] @?= 1,
      testCase "fun2 1" $ fun2 1 @?= 0
    ]
