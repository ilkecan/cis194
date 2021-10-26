import Homework4
  ( fun1,
    fun2,
  )
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

fun1Baseline :: [Integer] -> Integer
fun1Baseline [] = 1
fun1Baseline (x : xs)
  | even x = (x - 2) * fun1Baseline xs
  | otherwise = fun1Baseline xs

prop_1 :: [Integer] -> Bool
prop_1 ns = fun1 ns == fun1Baseline ns

fun2Baseline :: Integer -> Integer
fun2Baseline 1 = 0
fun2Baseline n
  | even n = n + fun2Baseline (n `div` 2)
  | otherwise = fun2Baseline (3 * n + 1)

prop_2 :: QC.Positive Integer -> Bool
prop_2 (QC.Positive n) = fun2 n == fun2Baseline n

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck properties"
    [ QC.testProperty "fun1 == fun1Baseline " prop_1,
      QC.testProperty "fun2 == fun2Baseline" prop_2
    ]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ -- Exercise1
      testCase "fun1 []" $ fun1 [] @?= 1,
      testCase "fun2 1" $ fun2 1 @?= 0
    ]
